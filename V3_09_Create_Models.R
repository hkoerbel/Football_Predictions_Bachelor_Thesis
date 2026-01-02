# What this script is for:
# In this script the models for predicting the outcome of the football games are trained.
#
# For each train/test file pair:
#   * XGBoost multiclass classifier
#   * randomForest (randomForest package) classifier
#   * Random Forest (ranger package) classifier
#
# Features:
#   - Exclude: HomeTeam, AwayTeam, Date (do not use them as explanatory variables)
# Target:
#   - FTR in {H, D, A} (home win, draw, away win)
#
# Metrics on test set:
#   * RPS
#   * Accuracy (% correct)
#   * number of predicted H/D/A
#   * number of true H/D/A
#
# Outputs:
#   * models/*.rds (one file per model)
#   * model_results.csv (evaluation summary)

# -----------------------------
# Package setup
# -----------------------------
required_pkgs <- c("xgboost", "randomForest", "ranger")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

suppressPackageStartupMessages({
  library(xgboost)
  library(randomForest)
  library(ranger)
})

# Global seed (for reproducibility)
set.seed(123)

# -----------------------------
# Helper: read CSV robustly
# -----------------------------
read_csv_safe <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  return(df)
}

# -----------------------------
# Helper: simple train-based imputer
# All checks in the previous scripts resulted in no missing values, therefore
# no values should be imputed. After brainstorming with ChatGPT I still decided
# to include this imputer as a safety approach. If for whatever reason there 
# would appear NAs, this imputer takes care of them, by using values from the
# according train data.
# -----------------------------
make_imputer <- function(train_df) {
  # Creates a list of imputation values for each column:
  #   * Numeric/integer: median
  #   * Factor/character: mode (most frequent)
  #   * Logical: mode
  # Also stores training factor levels to enforce consistency on test data, in 
  # case an unseen factor appears in the test data.
  imp <- list(
    numeric_median = list(),
    factor_mode = list(),
    logical_mode = list(),# should never occur, because there should be no logical variables
    factor_levels = list()
  )

  for (nm in names(train_df)) {
    x <- train_df[[nm]]

    # Treat character as factor for consistent downstream handling.
    if (is.character(x)) x <- as.factor(x)

    if (is.factor(x)) {
      # Store factor levels from training data
      imp$factor_levels[[nm]] <- levels(x)

      # Mode (most frequent non-NA)
      tab <- table(x, useNA = "no")
      imp$factor_mode[[nm]] <- if (length(tab) == 0) NA else names(tab)[which.max(tab)]

    } else if (is.logical(x)) {
      tab <- table(x, useNA = "no")
      imp$logical_mode[[nm]] <- if (length(tab) == 0) FALSE else as.logical(names(tab)[which.max(tab)])

    }
      # Median
      else if (is.numeric(x) || is.integer(x)) {
      med <- suppressWarnings(stats::median(x, na.rm = TRUE))
      if (is.na(med)) med <- 0
      imp$numeric_median[[nm]] <- med

    } else {
      # Fallback: coerce to factor and treat like factor
      xf <- as.factor(as.character(x))
      imp$factor_levels[[nm]] <- levels(xf)
      tab <- table(xf, useNA = "no")
      imp$factor_mode[[nm]] <- if (length(tab) == 0) NA else names(tab)[which.max(tab)]
    }
  }

  return(imp)
}

# This function actually applies the imputer. It is called on all datasets, but
# only changes anything if NAs appear or the test set has factors the train set 
# does not. It receives the dataframe, that shall be imputed and an imputer
# object from "make_imputer()", which holds imputation values for all columns.
apply_imputer <- function(df, imp) {
  # Create a copy of the input to not alter the original
  out <- df

  # Loop over all columns of the data frame
  for (nm in names(out)) {
    x <- out[[nm]]

    # Character -> factor
    if (is.character(x)) x <- as.factor(x)

    if (is.factor(x)) {
      # Align factor levels to training levels; unseen levels in test data become NA
      if (!is.null(imp$factor_levels[[nm]])) {
        x <- factor(as.character(x), levels = imp$factor_levels[[nm]])
      }
      # Impute NA with training mode
      mode_val <- imp$factor_mode[[nm]]
      if (!is.null(mode_val) && !is.na(mode_val)) {
        x[is.na(x)] <- mode_val # this line imputes NAs
      }
      out[[nm]] <- x # here the copy of the data frame handed over to the function is updated with the imputed column

    } else if (is.logical(x)) {
      mode_val <- imp$logical_mode[[nm]]
      if (!is.null(mode_val)) x[is.na(x)] <- mode_val # this line imputes NAs
      out[[nm]] <- x # here the copy of the data frame handed over to the function is updated with the imputed column

    } else if (is.numeric(x) || is.integer(x)) {
      med <- imp$numeric_median[[nm]]
      if (!is.null(med)) x[is.na(x)] <- med # this line imputes NAs
      out[[nm]] <- x # here the copy of the data frame handed over to the function is updated with the imputed column

    } else {
      # Fallback: coerce to factor and treat like factor
      xf <- as.factor(as.character(x))
      if (!is.null(imp$factor_levels[[nm]])) {
        xf <- factor(as.character(xf), levels = imp$factor_levels[[nm]])
      }
      mode_val <- imp$factor_mode[[nm]]
      if (!is.null(mode_val) && !is.na(mode_val)) {
        xf[is.na(xf)] <- mode_val # this line imputes NAs
      }
      out[[nm]] <- xf # here the copy of the data frame handed over to the function is updated with the imputed column
    }
  }

  return(out) # return the updated copy of the handed over data frame
}

# -----------------------------
# Metric: RPS
# -----------------------------
compute_rps <- function(probs, y_true) {
  # probs: matrix/data frame with columns named "H","D","A"
  # y_true: factor with levels including H,D,A
  M <- length(y_true) # number of games played

  yH <- as.integer(y_true == "H")
  yA <- as.integer(y_true == "A")

  pH <- probs[, "H"]
  pA <- probs[, "A"]

  rps <- (1 / (2 * M)) * sum((pH - yH)^2 + (pA - yA)^2)
  return(as.numeric(rps))
}

# -----------------------------
# Prepare data: drop columns, set target, impute
# -----------------------------
prepare_train_test <- function(train_path, test_path, drop_cols = c("HomeTeam", "AwayTeam", "Date")) {
  train_raw <- read_csv_safe(train_path) #read in train data
  test_raw  <- read_csv_safe(test_path) #read in test data

  #Sanity check: Is there a FTR column in both train and test data?
  if (!("FTR" %in% names(train_raw)) || !("FTR" %in% names(test_raw))) {
    stop("FTR column is missing in: ", train_path, " or ", test_path)
  }

  # Fixed target level order for stability
  train_raw$FTR <- factor(train_raw$FTR, levels = c("H", "D", "A"))
  test_raw$FTR  <- factor(test_raw$FTR,  levels = c("H", "D", "A"))

  # Drop columns HomeTeam, AwayTeam, Date
  drop_existing <- intersect(drop_cols, names(train_raw))
  train_df <- train_raw[, setdiff(names(train_raw), drop_existing), drop = FALSE]
  test_df  <- test_raw[,  setdiff(names(test_raw),  drop_existing), drop = FALSE]

  # Set the explained variable
  y_train <- train_df$FTR
  y_test  <- test_df$FTR

  # Set the explanatory variables
  x_train <- train_df[, setdiff(names(train_df), "FTR"), drop = FALSE]
  x_test  <- test_df[,  setdiff(names(test_df),  "FTR"), drop = FALSE]

  # Convert character predictors to factor (needed for consistent encoding)
  for (nm in names(x_train)) if (is.character(x_train[[nm]])) x_train[[nm]] <- as.factor(x_train[[nm]])
  for (nm in names(x_test))  if (is.character(x_test[[nm]]))  x_test[[nm]]  <- as.factor(x_test[[nm]])

  # Run the imputer using training statistics only
  # Values are only imputed, if necessary 
  imp <- make_imputer(x_train)
  x_train_imp <- apply_imputer(x_train, imp)
  x_test_imp  <- apply_imputer(x_test,  imp)

  list(
    x_train = x_train_imp,
    y_train = y_train,
    x_test  = x_test_imp,
    y_test  = y_test,
    imputer = imp
  )
}

# -----------------------------
# XGBoost: one-hot encoding based on training levels
# (There should be not dummy variables, because all explanatory variables are
# numeric. I still wanted to work like there were factorial variables, which would
# make one-hot encoded dummy variables necessary, in case I ever add such variables
# to the data.)
# -----------------------------
make_model_matrix_pair <- function(x_train, x_test) {
  form <- stats::as.formula("~ . - 1")
  # creates a formula with the following properties:
  # ~: split left side (target variable) form right side (explanatory variables)
  # .: take all columns (explanatory variables)
  # -1: no intercept

  # Build terms/xlevels from training so test encoding matches training
  # model.frame builds a table with the variables the formula demands (all)
  mf_train <- stats::model.frame(form, data = x_train, na.action = na.pass)
  # terms() creates a metadata object that describes which variables are in the model
  terms_obj <- stats::terms(mf_train)
  # .getXlevels collects all factor levels from all columns
  xlev <- .getXlevels(terms_obj, mf_train)

  # create a matrix which an XGBoost model can use, with the properties obtained before
  mm_train <- stats::model.matrix(terms_obj, data = mf_train)
  # Get the encoding rules for factor variables from the matrix of the train set,
  # so they can be handed over to the train data later.
  contr <- attr(mm_train, "contrasts")

  # Apply training xlevels to test
  mf_test <- stats::model.frame(terms_obj, data = x_test, xlev = xlev, na.action = na.pass)
  mm_test <- stats::model.matrix(terms_obj, data = mf_test, contrasts.arg = contr)

  # Align columns defensively (should match if levels are consistent)
  train_cols <- colnames(mm_train)
  test_cols  <- colnames(mm_test)
  all_cols <- union(train_cols, test_cols)

  # Create empty matrices with all columns
  mm_train2 <- matrix(0, nrow = nrow(mm_train), ncol = length(all_cols), dimnames = list(NULL, all_cols))
  mm_test2  <- matrix(0, nrow = nrow(mm_test),  ncol = length(all_cols), dimnames = list(NULL, all_cols))

  # Paste the columns from before into the new matrices.
  # mm_train2 and mm_test2 should not be different to mm_train and mm_test, because
  # they should have the same columns, therefore no new empty columns should be added.
  # This is a safety approach.
  mm_train2[, train_cols] <- mm_train
  mm_test2[,  test_cols]  <- mm_test
  
  # Now the train and test data are definitely normalised to identical feature columns.

  # Collect the information for the XGBoost model.
  list(
    train = mm_train2,
    test  = mm_test2,
    xgb_encoding = list(
      terms = terms_obj,
      xlevels = xlev,
      contrasts = contr,
      feature_names = all_cols
    )
  )
}

# Function to train XGBoost models.
train_xgboost_model <- function(mm_train, y_train) {
  # Transform the levels (H,D,A) to labels (0,1,2) for the XGBoost model, and
  # ensure the datatype stays integer with the "L"
  label <- as.integer(y_train) - 1L
  # Combine the explanatory variables and the explained variable in DMatrix,
  # for the XGBoost.
  dtrain <- xgb.DMatrix(data = mm_train, label = label)

  # Reproducibility:
  # - seed: fixed seed inside XGBoost
  # - nthread = 1: single-threaded training for cleaner reproducibility
  params <- list(
    objective = "multi:softprob", # multiclass classification with probabilities for different classes
    num_class = 3, # number of classes
    eval_metric = "mlogloss", # optimise multiclass log loss 
    eta = 0.05, # learning rate
    max_depth = 6, # maximum tree depth
    subsample = 0.8, # only use random 80% subsample of rows per tree
    colsample_bytree = 0.8, # only use random 80% of features per tree
    min_child_weight = 1,
    seed = 123, # for reproducibility
    nthread = 1 # no parallel calculations for cleaner reproducibility
  )

  # Here the model is actually trained.
  model <- xgb.train(
    params = params, # above defined parameters
    data = dtrain, # train data
    nrounds = 300, # number of boosting iterations
    verbose = 0 # no output in console
  )

  return(model)
}

# Predict on the test data and get probabilities for all 3 possible outcomes.
predict_xgboost_probs <- function(model, mm_test) {
  # Transform test data into DMatrix.
  dtest <- xgb.DMatrix(data = mm_test)
  # Predict with the created model and store all predicted probabilities in 
  # the vector raw.
  raw <- predict(model, dtest)  # length = nrow * 3
  # Turn the vector with the predicted probabilities into a matrix with 3 columns.
  probs <- matrix(raw, ncol = 3, byrow = TRUE)
  # Give the matrix the right column names.
  colnames(probs) <- c("H", "D", "A")
  return(probs)
}

# -----------------------------
# randomForest (randomForest package)
# -----------------------------
train_rf_model <- function(x_train, y_train) {
  p <- ncol(x_train) # number of predictors
  mtry <- max(1, floor(sqrt(p))) # number of features considered per split (ensure it never gets 0 with max(1,...))

  # Train the model
  model <- randomForest::randomForest(
    x = x_train, # predictors
    y = y_train, # explained
    ntree = 500, # number of trees
    mtry = mtry # mtry as defined above
  )
  return(model)
}

# Predict the probabilities of the 3 different outcomes.
predict_rf_probs <- function(model, x_test) {
  probs <- predict(model, newdata = x_test, type = "prob")
  probs <- probs[, c("H", "D", "A"), drop = FALSE]
  return(as.matrix(probs))
}

# -----------------------------
# ranger (ranger package)
# -----------------------------
train_ranger_model <- function(x_train, y_train) {
  # Column wise bind the target variable as a new column (FTR) on the explanatory 
  # variables, so one dataframe can be handed over to the Random Forest.
  df <- cbind(FTR = y_train, x_train) 
  p <- ncol(x_train) # number of predictors
  mtry <- max(1, floor(sqrt(p))) # number of features considered per split (ensure it never gets 0 with max(1,...))

  # Train the model here.
  model <- ranger::ranger(
    dependent.variable.name = "FTR", # explained
    data = df,
    probability = TRUE, # get probabilities of classes
    num.trees = 500, # number of trees
    mtry = mtry, # mtry as defined above
    seed = 123 # ranger has its own seed argument for reproducibility
  )
  return(model)
}

# Get prediction probabilities.
predict_ranger_probs <- function(model, x_test) {
  # Create object with the probabilities of the 3 outcomes.
  pred <- predict(model, data = x_test)
  # Select the probabilites only.
  probs <- pred$predictions
  # Structure columns in the given sequence.
  probs <- probs[, c("H", "D", "A"), drop = FALSE]
  return(as.matrix(probs))
}

# -----------------------------
# Evaluation helper
# Requires the predicted probabilites and the true results
# -----------------------------
evaluate_probs <- function(probs, y_test) {
  # Get the index of the highest probability for all rows.
  pred_idx <- max.col(probs, ties.method = "first")
  # Transform the indices into the right class name (H,D,A).
  pred_cls <- factor(colnames(probs)[pred_idx], levels = c("H", "D", "A"))

  # Calculate the accuracy and the RPS of the predictions.
  acc <- mean(pred_cls == y_test) * 100
  rps <- compute_rps(probs, y_test)

  # Count how often each outcome is predicted and how often they really occur. 
  pred_counts <- table(pred_cls)
  true_counts <- table(y_test)

  # Since table() only gets levels that appear at least once it would not return
  # a number for predictions that do not occur. With this the function the count
  # of all desired levels can be checked in case a level does not appear a "0" (integer)
  # is returned.
  get_count <- function(tab, lvl) if (lvl %in% names(tab)) as.integer(tab[[lvl]]) else 0L

  # Collect metrics.
  list(
    accuracy = as.numeric(acc),
    rps = as.numeric(rps),
    pred_H = get_count(pred_counts, "H"),
    pred_D = get_count(pred_counts, "D"),
    pred_A = get_count(pred_counts, "A"),
    true_H = get_count(true_counts, "H"),
    true_D = get_count(true_counts, "D"),
    true_A = get_count(true_counts, "A"),
    n_test = length(y_test)
  )
}

# -----------------------------
# Define a list with all dataset pairs
# -----------------------------
get_dataset_pairs <- function() {
  pairs <- list()

  # 1) data_(-)FormParameter_(-)BettingQuotes (single pair)
  dir1 <- "data_(-)FormParameter_(-)BettingQuotes"
  train1 <- file.path(dir1, "all_variables_(-)form_parameter_(-)betting_quotes_train_data.csv")
  test1  <- file.path(dir1, "all_variables_(-)form_parameter_(-)betting_quotes_test_data.csv")
  pairs[[length(pairs) + 1]] <- list(train = train1, test = test1, suffix = "av_(-)fp_(-)bq")

  # 2) data_(-)FormParameter_(+)BettingQuotes (single pair)
  dir2 <- "data_(-)FormParameter_(+)BettingQuotes"
  train2 <- file.path(dir2, "all_variables_(-)form_parameter_(+)betting_quotes_train_data.csv")
  test2  <- file.path(dir2, "all_variables_(-)form_parameter_(+)betting_quotes_test_data.csv")
  pairs[[length(pairs) + 1]] <- list(train = train2, test = test2, suffix = "av_(-)fp_(+)bq")

  # 3) data_(+)FormParameter_(-)BettingQuotes (multiple pairs)
  dir3 <- "data_(+)FormParameter_(-)BettingQuotes"
  pat3 <- "^all_variables_\\(\\+\\)form_parameter_(\\d{3})_\\(-\\)betting_quotes_train_data\\.csv$"
  train_files3 <- sort(list.files(dir3, pattern = pat3, full.names = FALSE))
  for (f in train_files3) {
    m <- regexec(pat3, f)
    g <- regmatches(f, m)[[1]]
    xxx <- g[2]
    trainp <- file.path(dir3, f)
    testp  <- file.path(dir3, sub("_train_data\\.csv$", "_test_data.csv", f))
    pairs[[length(pairs) + 1]] <- list(train = trainp, test = testp, suffix = paste0("av_(+)fp_", xxx, "_(-)bq"))
  }

  # 4) data_(+)FormParameter_(+)BettingQuotes (multiple pairs)
  dir4 <- "data_(+)FormParameter_(+)BettingQuotes"
  pat4 <- "^all_variables_\\(\\+\\)form_parameter_(\\d{3})_\\(\\+\\)betting_quotes_train_data\\.csv$"
  train_files4 <- sort(list.files(dir4, pattern = pat4, full.names = FALSE))
  for (f in train_files4) {
    m <- regexec(pat4, f)
    g <- regmatches(f, m)[[1]]
    xxx <- g[2]
    trainp <- file.path(dir4, f)
    testp  <- file.path(dir4, sub("_train_data\\.csv$", "_test_data.csv", f))
    pairs[[length(pairs) + 1]] <- list(train = trainp, test = testp, suffix = paste0("av_(+)fp_", xxx, "_(+)bq"))
  }

  # 5) data_onlyFormParameter (multiple pairs)
  dir5 <- "data_onlyFormParameter"
  pat5 <- "^gamma_(\\d{3})_train\\.csv$"
  train_files5 <- sort(list.files(dir5, pattern = pat5, full.names = FALSE))
  for (f in train_files5) {
    m <- regexec(pat5, f)
    g <- regmatches(f, m)[[1]]
    xxx <- g[2]
    trainp <- file.path(dir5, f)
    testp  <- file.path(dir5, sub("_train\\.csv$", "_test.csv", f))
    pairs[[length(pairs) + 1]] <- list(train = trainp, test = testp, suffix = paste0("only_fp_", xxx))
  }

  return(pairs)
}

# -----------------------------
# Main loop
# -----------------------------
# Ensure output folder exists (relative to current working directory)
dir.create("models", showWarnings = FALSE)

# Collect the dataset pairs.
pairs <- get_dataset_pairs()

# Initialzse a data frame for the results.
results <- data.frame(
  dataset_suffix = character(),
  model_name = character(),
  model_type = character(),
  accuracy = numeric(),
  rps = numeric(),
  pred_H = integer(),
  pred_D = integer(),
  pred_A = integer(),
  true_H = integer(),
  true_D = integer(),
  true_A = integer(),
  n_test = integer(),
  stringsAsFactors = FALSE
)

# Iterate over all dataset pairs and create the models by calling the before 
# defined functions.
for (i in seq_along(pairs)) {
  pair <- pairs[[i]]

  # Sanity check: Do the required files exist?
  if (!file.exists(pair$train)) {
    warning("Missing train file: ", pair$train, " (skipping)")
    next
  }
  if (!file.exists(pair$test)) {
    warning("Missing test file: ", pair$test, " (skipping)")
    next
  }

  message("Processing dataset: ", pair$suffix)

  # Prepare data.
  dat <- prepare_train_test(pair$train, pair$test)
  x_train <- dat$x_train
  y_train <- dat$y_train
  x_test  <- dat$x_test
  y_test  <- dat$y_test

  # True counts in test (same for all models)
  true_counts <- table(y_test)
  trueH <- if ("H" %in% names(true_counts)) as.integer(true_counts[["H"]]) else 0L
  trueD <- if ("D" %in% names(true_counts)) as.integer(true_counts[["D"]]) else 0L
  trueA <- if ("A" %in% names(true_counts)) as.integer(true_counts[["A"]]) else 0L

  # -------------------------
  # 1) XGBoost
  # -------------------------
  mm <- make_model_matrix_pair(x_train, x_test)

  set.seed(123)
  # Send data to XGBoost functions, create model, calculate probabilities and 
  # evaluate performance.
  xgb_model <- train_xgboost_model(mm$train, y_train)
  xgb_probs <- predict_xgboost_probs(xgb_model, mm$test)
  xgb_eval <- evaluate_probs(xgb_probs, y_test)

  # Give current model the right name.
  xgb_name <- paste0("xgboost_", pair$suffix)
  # Safe the model with its metadata as RDS file.
  saveRDS(
    list(
      model = xgb_model,
      model_type = "xgboost",
      suffix = pair$suffix,
      drop_cols = c("HomeTeam", "AwayTeam", "Date"), # to remember which columns to drop before modeling
      imputer = dat$imputer,
      xgb_encoding = mm$xgb_encoding, # meta data for one-hot encoding
      target_levels = c("H", "D", "A")
    ),
    file = file.path("models", paste0(xgb_name, ".rds"))
  )

  # Add new rows with the results of the model to "results".
  results <- rbind(
    results,
    data.frame(
      dataset_suffix = pair$suffix,
      model_name = xgb_name,
      model_type = "xgboost",
      accuracy = xgb_eval$accuracy,
      rps = xgb_eval$rps,
      pred_H = xgb_eval$pred_H,
      pred_D = xgb_eval$pred_D,
      pred_A = xgb_eval$pred_A,
      true_H = trueH,
      true_D = trueD,
      true_A = trueA,
      n_test = xgb_eval$n_test,
      stringsAsFactors = FALSE
    )
  )

  # -------------------------
  # 2) randomForest (randomForest pkg)
  # -------------------------
  set.seed(123)
  # Send data to Random Forest functions, create model, calculate probabilities and 
  # evaluate performance.
  rf_model <- train_rf_model(x_train, y_train)
  rf_probs <- predict_rf_probs(rf_model, x_test)
  rf_eval <- evaluate_probs(rf_probs, y_test)

  # Give current model the right name.
  rf_name <- paste0("rf_", pair$suffix)
  # Safe the model with its metadata as RDS file.
  saveRDS(
    list(
      model = rf_model,
      model_type = "randomForest",
      suffix = pair$suffix,
      drop_cols = c("HomeTeam", "AwayTeam", "Date"),
      imputer = dat$imputer,
      target_levels = c("H", "D", "A")
    ),
    file = file.path("models", paste0(rf_name, ".rds"))
  )

  # Add new rows with the results of the model to "results".
  results <- rbind(
    results,
    data.frame(
      dataset_suffix = pair$suffix,
      model_name = rf_name,
      model_type = "randomForest",
      accuracy = rf_eval$accuracy,
      rps = rf_eval$rps,
      pred_H = rf_eval$pred_H,
      pred_D = rf_eval$pred_D,
      pred_A = rf_eval$pred_A,
      true_H = trueH,
      true_D = trueD,
      true_A = trueA,
      n_test = rf_eval$n_test,
      stringsAsFactors = FALSE
    )
  )

  # -------------------------
  # 3) ranger
  # -------------------------
  set.seed(123)
  # Send data to Ranger functions, create model, calculate probabilities and 
  # evaluate performance.
  ranger_model <- train_ranger_model(x_train, y_train)
  ranger_probs <- predict_ranger_probs(ranger_model, x_test)
  ranger_eval <- evaluate_probs(ranger_probs, y_test)

  # Give current model the right name.
  ranger_name <- paste0("ranger_", pair$suffix)
  # Safe the model with its metadata as RDS file.
  saveRDS(
    list(
      model = ranger_model,
      model_type = "ranger",
      suffix = pair$suffix,
      drop_cols = c("HomeTeam", "AwayTeam", "Date"),
      imputer = dat$imputer,
      target_levels = c("H", "D", "A")
    ),
    file = file.path("models", paste0(ranger_name, ".rds"))
  )

  # Add new rows with the results of the model to "results".
  results <- rbind(
    results,
    data.frame(
      dataset_suffix = pair$suffix,
      model_name = ranger_name,
      model_type = "ranger",
      accuracy = ranger_eval$accuracy,
      rps = ranger_eval$rps,
      pred_H = ranger_eval$pred_H,
      pred_D = ranger_eval$pred_D,
      pred_A = ranger_eval$pred_A,
      true_H = trueH,
      true_D = trueD,
      true_A = trueA,
      n_test = ranger_eval$n_test,
      stringsAsFactors = FALSE
    )
  )
}

# -----------------------------
# Save results
# -----------------------------
#write.csv(results, "model_results.csv", row.names = FALSE)

message("Done. Results saved to: model_results.csv")
message("Models saved in folder: models/")
