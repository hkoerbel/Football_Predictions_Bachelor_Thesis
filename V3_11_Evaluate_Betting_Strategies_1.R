# What this script is for:
# Evaluating betting strategies.
#
# Strategy 1 (all models + Bookies):
#   - Stake = 1 unit per match.
#   - Place exactly one bet per match:
#       * Models: bet on argmax(probabilities)
#       * Bookies: bet on the outcome with the lowest odds
#                 (highest implied probability)
#
# Strategy 2 (all models), evaluated for tau in [0, 4] with step 0.01:
#   - Strategy 2a ("ONE BET"): max 1 bet per match
#       -> bet 1 unit on the outcome with the highest EV if maxEV > tau
#   - Strategy 2b ("MULTI BET"): possibly multiple bets per match
#       -> bet 1 unit on each outcome with EV(outcome) > tau
#
# Odds handling:
#   - Prefer odds (B365H/B365D/B365A) in each test file if present
#   - Fill missing/invalid odds from master file:
#       FootballUK_data_(+)BettingQuotes.csv
#     matched by (HomeTeam, AwayTeam, Date), with robust canonical date parsing.
#
# Outputs:
#   - Strategy1_Profit_AllModels.csv
#   - Strategy1_Profit_Bookies.csv
#   - Strategy2_OneBet_AllModels.csv
#   - Strategy2_MultiBet_AllModels.csv
#   - Strategy2_Summary_BestModels.csv
#   - Strategy2_Summary_BestModels_OnlyNames.csv
#   - Profit-vs-tau PNG plots for the 2 selected best models (both variants)
#     with 95% CI bands for Net Profit.

# =========================================================
# Load packages
# =========================================================
required_pkgs <- c("xgboost", "randomForest", "ranger")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(xgboost)
  library(randomForest)
  library(ranger)
})
set.seed(123)

# =========================================================
# Helpers
# =========================================================
read_csv_safe <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# ---------------------------------------------------------
# Robust date canonicalization for easy joins of odds
# ---------------------------------------------------------
canonicalize_date <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x0 <- trimws(as.character(x))
  # If format is already "Date" -> return
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  
  # Formats to check.
  fmts <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d.%m.%Y",
    "%Y/%m/%d", "%d-%m-%Y", "%m-%d-%Y", "%Y.%m.%d"
  )
  
  # Initialize out with NAs (will be returned later).
  out <- rep(NA_character_, length(x0))
  # Iteratively parse entries.
  for (fmt in fmts) {
    # Not been parsed yet (NA) and not empty (nzchar)
    idx <- is.na(out) & nzchar(x0)
    # Break if no TRUE elements in idx anymore (all has been parsed)
    if (!any(idx)) break
    # Try to parse with the current format.
    parsed <- as.Date(x0[idx], format = fmt)
    # Check if entries are parsed succesfully. 
    ok <- !is.na(parsed)
    # Write canonical form in "out" for successfully parsed values.
    if (any(ok)) out[idx][ok] <- format(parsed[ok], "%Y-%m-%d")
  }
  
  # If parsing did not work keep original.
  out[is.na(out) | !nzchar(out)] <- x0[is.na(out) | !nzchar(out)]
  out
}

# ---------------------------------------------------------
# This function applies the imputer. It only changes anything if NAs appear or
# the test set has factors the train set does not. It receives the dataframe, 
# that shall be imputed and an imputer object, which holds imputation values 
# for all columns.
# ---------------------------------------------------------
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
      if (!is.null(mode_val) && !is.na(mode_val)) x[is.na(x)] <- mode_val # this line imputes NAs
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
      # Fallback: coerce to factor and treat like factor.
      xf <- as.factor(as.character(x))
      if (!is.null(imp$factor_levels[[nm]])) {
        xf <- factor(as.character(xf), levels = imp$factor_levels[[nm]])
      }
      mode_val <- imp$factor_mode[[nm]]
      if (!is.null(mode_val) && !is.na(mode_val)) xf[is.na(xf)] <- mode_val # this line imputes NAs
      out[[nm]] <- xf # here the copy of the data frame handed over to the function is updated with the imputed column
    }
  }
  out # return the updated copy of the handed over data frame
}

# =========================================================
# Discover test files by suffix and create a list with them.
# (The function works the same as "get_dataset_pairs()" from
# "V3_09_Create_Models.R", therefore they received the same
# name, even though here only the test files are retrieved.)
# =========================================================
get_dataset_pairs <- function() {
  pairs <- list()
  
  # 1) data_(-)FormParameter_(-)BettingQuotes
  dir1 <- "data_(-)FormParameter_(-)BettingQuotes"
  pairs[[length(pairs) + 1]] <- list(
    test = file.path(dir1, "all_variables_(-)form_parameter_(-)betting_quotes_test_data.csv"),
    suffix = "av_(-)fp_(-)bq"
  )
  
  # 2) data_(-)FormParameter_(+)BettingQuotes
  dir2 <- "data_(-)FormParameter_(+)BettingQuotes"
  pairs[[length(pairs) + 1]] <- list(
    test = file.path(dir2, "all_variables_(-)form_parameter_(+)betting_quotes_test_data.csv"),
    suffix = "av_(-)fp_(+)bq"
  )
  
  # 3) data_(+)FormParameter_(-)BettingQuotes
  dir3 <- "data_(+)FormParameter_(-)BettingQuotes"
  pat3 <- "^all_variables_\\(\\+\\)form_parameter_(\\d{3})_\\(-\\)betting_quotes_test_data\\.csv$"
  for (f in sort(list.files(dir3, pattern = pat3, full.names = FALSE))) {
    xxx <- regmatches(f, regexec(pat3, f))[[1]][2]
    pairs[[length(pairs) + 1]] <- list(
      test = file.path(dir3, f),
      suffix = paste0("av_(+)fp_", xxx, "_(-)bq")
    )
  }
  
  # 4) data_(+)FormParameter_(+)BettingQuotes
  dir4 <- "data_(+)FormParameter_(+)BettingQuotes"
  pat4 <- "^all_variables_\\(\\+\\)form_parameter_(\\d{3})_\\(\\+\\)betting_quotes_test_data\\.csv$"
  for (f in sort(list.files(dir4, pattern = pat4, full.names = FALSE))) {
    xxx <- regmatches(f, regexec(pat4, f))[[1]][2]
    pairs[[length(pairs) + 1]] <- list(
      test = file.path(dir4, f),
      suffix = paste0("av_(+)fp_", xxx, "_(+)bq")
    )
  }
  
  # 5) data_onlyFormParameter
  dir5 <- "data_onlyFormParameter"
  pat5 <- "^gamma_(\\d{3})_test\\.csv$"
  for (f in sort(list.files(dir5, pattern = pat5, full.names = FALSE))) {
    xxx <- regmatches(f, regexec(pat5, f))[[1]][2]
    pairs[[length(pairs) + 1]] <- list(
      test = file.path(dir5, f),
      suffix = paste0("only_fp_", xxx)
    )
  }
  
  pairs
}

# This function finds the paths to the test files of the models, based on the
# model suffix.
find_test_path_by_suffix <- function(suffix, pairs) {
  # Find all elements with the right suffix.
  hit <- vapply(pairs, function(z) identical(z$suffix, suffix), logical(1))
  # In case of no hits return NA.
  if (!any(hit)) return(NA_character_)
  # Retrieve the path of the test file with the right suffix (in case of more
  # fitting elements take the first).
  pairs[[which(hit)[1]]]$test
}

# =========================================================
# Create a master odds lookup table to fill missing odds in
# test data (should never be necessary but works as an additional
# safety approach).
# =========================================================
build_master_odds_lookup <- function(master_path) {
  # Sanity check: Does the file exist?
  if (!file.exists(master_path)) stop("Master odds file not found: ", master_path)
  # Read in file
  master <- read_csv_safe(master_path)
  
  # Check for required columns in the file.
  needed <- c("HomeTeam", "AwayTeam", "Date", "B365H", "B365D", "B365A")
  miss <- setdiff(needed, names(master))
  if (length(miss) > 0) stop("Master odds file missing columns: ", paste(miss, collapse = ", "))
  
  # Transform date column into canonical format, using the before defined function.
  mdate <- canonicalize_date(master$Date)
  # Create a key for each game with separation "___"
  key <- paste(master$HomeTeam, master$AwayTeam, mdate, sep = "___")
  
  # If there are duplicates (should not happen), only the first entry is kept.
  keep <- !duplicated(key)
  # Return data frame with the key and the odds.
  data.frame(
    key = key[keep],
    B365H = master$B365H[keep],
    B365D = master$B365D[keep],
    B365A = master$B365A[keep],
    stringsAsFactors = FALSE
  )
}

# =========================================================
# Prepare test data for prediction + attach odds (test odds first, then master fill)
# =========================================================
prepare_test_for_model <- function(test_path, bundle, master_odds_df) {
  # Read in file.
  df <- read_csv_safe(test_path)
  
  # Sanity check: Are all columns for matching the odds available? 
  match_cols <- c("HomeTeam", "AwayTeam", "Date")
  if (!all(match_cols %in% names(df))) {
    stop("Test file missing HomeTeam/AwayTeam/Date -> cannot match odds: ", test_path)
  }
  # Sanity check: Is FTR available?
  if (!("FTR" %in% names(df))) stop("FTR missing in ", test_path)
  # Transform FTR to factor.
  df$FTR <- factor(df$FTR, levels = c("H", "D", "A"))
  
  # Canonicalize the date column.
  tdate <- canonicalize_date(df$Date)
  # Create unique keys per match (same form as in build_master_odds_lookup().
  tkey <- paste(df$HomeTeam, df$AwayTeam, tdate, sep = "___")
  
  # Initialize data frame with NA columns for odds.
  odds <- data.frame(
    B365H = rep(NA_real_, nrow(df)),
    B365D = rep(NA_real_, nrow(df)),
    B365A = rep(NA_real_, nrow(df))
  )
  
  # If the odds are in the test file -> fill the odds data frame with the odds
  # from the test file.
  odds_cols <- c("B365H", "B365D", "B365A")
  if (all(odds_cols %in% names(df))) {
    odds$B365H <- df$B365H
    odds$B365D <- df$B365D
    odds$B365A <- df$B365A
  }
  
  # Check if there is a fitting master entry and get indices as TRUE/FALSE. 
  idx <- match(tkey, master_odds_df$key)
  found <- !is.na(idx)
  
  # Get columns with missing or damaged (<= 0) odds, that exist in the master.
  fillH <- (is.na(odds$B365H) | odds$B365H <= 0) & found
  fillD <- (is.na(odds$B365D) | odds$B365D <= 0) & found
  fillA <- (is.na(odds$B365A) | odds$B365A <= 0) & found
  
  # Fill the odds (where missing) with values from the master.
  odds$B365H[fillH] <- master_odds_df$B365H[idx[fillH]]
  odds$B365D[fillD] <- master_odds_df$B365D[idx[fillD]]
  odds$B365A[fillA] <- master_odds_df$B365A[idx[fillA]]
  
  # Get columns that were dropped for training.
  drop_existing <- intersect(bundle$drop_cols, names(df))
  # Drop the same columns in the test set, so it fits the model.
  df2 <- df[, setdiff(names(df), drop_existing), drop = FALSE]
  
  # Split data into target and explanatory variables.
  y_test <- df2$FTR
  x_test <- df2[, setdiff(names(df2), "FTR"), drop = FALSE]
  
  # Transform explanatory character columns to factors (I only used numeric variables,
  # but wanted to keep the possibility to easily add character variables too.) 
  for (nm in names(x_test)) if (is.character(x_test[[nm]])) x_test[[nm]] <- as.factor(x_test[[nm]])
  # Send explanatory variables through the imputer to ensure the data is clean.
  # (Nothing should be imputed, because the data was clean in all checks -> safety approach)
  x_test <- apply_imputer(x_test, bundle$imputer)
  
  # Return list with explanatory variables, target variable and odds.
  list(x_test = x_test, y_test = y_test, odds = odds)
}

# =========================================================
# Prepare test data for BOOKIES baseline
# Works the same way as the function above, but here no explanatory
# variables are needed, because no model is fed to obtain a prediction.
# The prediction of the bookies is assumed to be the outcome with the lowest odds.
# =========================================================
prepare_test_for_bookies <- function(test_path, master_odds_df) {
  # Read in file.
  df <- read_csv_safe(test_path)
  
  # Sanity check: Are all columns for matching the odds available? 
  match_cols <- c("HomeTeam", "AwayTeam", "Date")
  if (!all(match_cols %in% names(df))) {
    stop("Test file missing HomeTeam/AwayTeam/Date -> cannot match odds: ", test_path)
  }
  # Sanity check: Is FTR available?
  if (!("FTR" %in% names(df))) stop("FTR missing in ", test_path)
  df$FTR <- factor(df$FTR, levels = c("H", "D", "A"))
  
  # Canonicalize the date column.
  tdate <- canonicalize_date(df$Date)
  # Create unique keys per match (same form as in build_master_odds_lookup().
  tkey <- paste(df$HomeTeam, df$AwayTeam, tdate, sep = "___")
  
  # Initialize data frame with NA columns for odds.
  odds <- data.frame(
    B365H = rep(NA_real_, nrow(df)),
    B365D = rep(NA_real_, nrow(df)),
    B365A = rep(NA_real_, nrow(df))
  )
  
  # If the odds are in the test file -> fill the odds data frame with the odds
  # from the test file.
  odds_cols <- c("B365H", "B365D", "B365A")
  if (all(odds_cols %in% names(df))) {
    odds$B365H <- df$B365H
    odds$B365D <- df$B365D
    odds$B365A <- df$B365A
  }
  
  # Check if there is a fitting master entry and get indices as TRUE/FALSE.
  idx <- match(tkey, master_odds_df$key)
  found <- !is.na(idx)
  
  # Get columns with missing or damaged (<= 0) odds, that exist in the master.
  fillH <- (is.na(odds$B365H) | odds$B365H <= 0) & found
  fillD <- (is.na(odds$B365D) | odds$B365D <= 0) & found
  fillA <- (is.na(odds$B365A) | odds$B365A <= 0) & found
  
  # Fill the odds (where missing) with values from the master.
  odds$B365H[fillH] <- master_odds_df$B365H[idx[fillH]]
  odds$B365D[fillD] <- master_odds_df$B365D[idx[fillD]]
  odds$B365A[fillA] <- master_odds_df$B365A[idx[fillA]]
  
  # Return a list with the target variable and the odds.
  list(y_test = df$FTR, odds = odds)
}

# =========================================================
# Predict probabilities from saved model bundle
# =========================================================
predict_probs_from_bundle <- function(bundle, x_test) {
  # Get the model type (xgboost, randomForest, ranger).
  mt <- bundle$model_type
  
  # xgboost needs more preparation than randomForest and ranger.
  if (mt == "xgboost") {
    # Collect the encoding metadata stored when training the model.
    enc <- bundle$xgb_encoding
    terms_obj <- enc$terms
    xlev <- enc$xlevels
    contr <- enc$contrasts
    feat_names <- enc$feature_names
    
    # Build model frame and matrix as in training.
    # model.frame builds the clean frame fitting to the terms.
    mf_test <- stats::model.frame(terms_obj, data = x_test, xlev = xlev, na.action = na.pass)
    # model.matrix builds the numeric design matrix (dummy variables) 
    mm_test <- stats::model.matrix(terms_obj, data = mf_test, contrasts.arg = contr)
    
    # Align columns with the training.
    # There could be dummy columns missing in the test data or there could be
    # columns the model does not know (should not be the case; safety approach).
    # Start by creating a matrix with all 0s in the according size.
    mm_aligned <- matrix(0, nrow = nrow(mm_test), ncol = length(feat_names),
                         dimnames = list(NULL, feat_names))
    # Get the columns that appear in both the test data and the models features.
    common <- intersect(colnames(mm_test), feat_names)
    # Only copy the columns that appear in both test data and the model into 
    # the matrix.
    mm_aligned[, common] <- mm_test[, common, drop = FALSE]
    
    # Call the model and get predictions.
    raw <- predict(bundle$model, xgb.DMatrix(mm_aligned))
    # Turn the response vector into a 3 column matrix.
    probs <- matrix(raw, ncol = 3, byrow = TRUE)
    # Assign the right names.
    colnames(probs) <- c("H", "D", "A")
    # Return the probabilities.
    return(probs)
  }
  
  if (mt == "randomForest") {
    # Call the model and get predictions.
    probs <- predict(bundle$model, newdata = x_test, type = "prob")
    # Ensure the right sequence of columns.
    probs <- probs[, c("H", "D", "A"), drop = FALSE]
    # Return the probabilities.
    return(as.matrix(probs))
  }
  
  if (mt == "ranger") {
    # Call the model and get predictions.
    pred <- predict(bundle$model, data = x_test)
    probs <- pred$predictions
    # Ensure the right sequence of columns.
    probs <- probs[, c("H", "D", "A"), drop = FALSE]
    # Return the probabilities.
    return(as.matrix(probs))
  }
  
  # Stop if model type does not fit.
  stop("Unknown model_type: ", mt)
}

# =========================================================
# Betting row validity check
# Filter for which match rows bets can be evaluated (they all should fit).
# =========================================================
valid_betting_rows <- function(y_true, odds_df) {
  # The true result is needed.
  ok_outcome <- !is.na(y_true)
  # Odds cannot be NA and have to be greater than 0.
  ok_odds <- !is.na(odds_df$B365H) & !is.na(odds_df$B365D) & !is.na(odds_df$B365A) &
    odds_df$B365H > 0 & odds_df$B365D > 0 & odds_df$B365A > 0
  ok_outcome & ok_odds
}

# =========================================================
# Strategy 1: Models (argmax prob) and Bookies (min odds)
# =========================================================
# Models
strategy1_backtest_model <- function(probs, y_true, odds_df) {
  # Get the rows which can be evaluated and filter probs, y_true and odds_df accordingly.
  keep <- valid_betting_rows(y_true, odds_df)
  probs <- probs[keep, , drop = FALSE]
  y_true <- y_true[keep]
  odds_df <- odds_df[keep, , drop = FALSE]
  
  # Get the index of the prediction.
  pred_idx <- max.col(probs, ties.method = "first")
  # Get the class of the prediction.
  pred_cls <- colnames(probs)[pred_idx]
  
  # Create a matrix with the odds.
  odds_mat <- cbind(H = odds_df$B365H, D = odds_df$B365D, A = odds_df$B365A)
  # Select the odds of the predicted outcome.
  chosen_odds <- odds_mat[cbind(seq_len(nrow(odds_mat)), match(pred_cls, colnames(odds_mat)))]
  
  # Check if the prediction was right.
  win <- (pred_cls == as.character(y_true))
  # If win == True -> payout equals the odds, otherwise 0
  payout <- ifelse(win, chosen_odds, 0)
  
  # Calculate profit
  n_bets <- length(payout)
  total_stake <- n_bets
  total_payout <- sum(payout)
  net_profit <- total_payout - total_stake
  
  # Collect metrics.
  data.frame(
    n_matches_used = n_bets,
    n_bets = n_bets,
    total_stake = total_stake,
    total_payout = total_payout,
    net_profit = net_profit,
    roi = if (total_stake > 0) net_profit / total_stake else NA_real_,
    return_factor = if (total_stake > 0) total_payout / total_stake else NA_real_
  )
}

# Bookies
strategy1_backtest_bookies <- function(y_true, odds_df) {
  # Get the rows which can be evaluated and filter y_true and odds_df accordingly.
  keep <- valid_betting_rows(y_true, odds_df)
  y_true <- y_true[keep]
  odds_df <- odds_df[keep, , drop = FALSE]
  
  # Create a matrix with the odds.
  odds_mat <- cbind(H = odds_df$B365H, D = odds_df$B365D, A = odds_df$B365A)
  
  # Get indices of lowest odds (1 -> row-wise)
  pred_idx <- apply(odds_mat, 1, which.min)
  # Get the class of the prediction.
  pred_cls <- colnames(odds_mat)[pred_idx]
  
  # Select the odds of the predicted outcome.
  chosen_odds <- odds_mat[cbind(seq_len(nrow(odds_mat)), match(pred_cls, colnames(odds_mat)))]
  
  # Check if the prediction was right.
  win <- (pred_cls == as.character(y_true))
  # If win == True -> payout equals the odds, otherwise 0
  payout <- ifelse(win, chosen_odds, 0)
  
  # Calculate profit
  n_bets <- length(payout)
  total_stake <- n_bets
  total_payout <- sum(payout)
  net_profit <- total_payout - total_stake
  
  # Collect metrics.
  data.frame(
    n_matches_used = n_bets,
    n_bets = n_bets,
    total_stake = total_stake,
    total_payout = total_payout,
    net_profit = net_profit,
    roi = if (total_stake > 0) net_profit / total_stake else NA_real_,
    return_factor = if (total_stake > 0) total_payout / total_stake else NA_real_
  )
}

# =========================================================
# Strategy 2a: ONE BET (max 1 bet per match)
# =========================================================
strategy2_curve_one_bet <- function(probs, y_true, odds_df, taus) {
  # Get the rows which can be evaluated and filter probs, y_true and odds_df accordingly.
  keep <- valid_betting_rows(y_true, odds_df)
  probs <- probs[keep, , drop = FALSE]
  y_true <- y_true[keep]
  odds_df <- odds_df[keep, , drop = FALSE]
  
  # Create a matrix with the odds.
  odds_mat <- cbind(H = odds_df$B365H, D = odds_df$B365D, A = odds_df$B365A)
  # Calculate the expected value for all possible bets.
  ev <- probs * odds_mat - 1
  
  # Get the index of the highest expected value.
  best_idx <- max.col(ev, ties.method = "first")
  # Get the according outcome class (H,D,A).
  best_outcome <- colnames(ev)[best_idx]
  # Get the highest expected value per match.
  best_ev <- ev[cbind(seq_len(nrow(ev)), best_idx)]
  # Get the odds of the outcome with the highest expected value for each match.
  best_odds <- odds_mat[cbind(seq_len(nrow(odds_mat)), match(best_outcome, colnames(odds_mat)))]
  
  # Transform outcomes to character and check if the outcome with the highest
  # expected value bet would be a win.
  y_char <- as.character(y_true)
  is_win <- (best_outcome == y_char)
  
  # Initialize a results data frame.
  res <- data.frame(
    tau = taus,
    n_bets = integer(length(taus)),
    total_stake = numeric(length(taus)),
    total_payout = numeric(length(taus)),
    net_profit = numeric(length(taus)),
    roi = numeric(length(taus)),
    return_factor = numeric(length(taus))
  )
  
  # Iterate over all taus [0;4] and calculate the returns for each tau.
  for (i in seq_along(taus)) {
    tau <- taus[i]
    # Place a bet if the expected value is greater than tau.
    do_bet <- best_ev > tau
    
    n_bets <- sum(do_bet)
    stake <- n_bets
    # If a bet is placed and won -> payout equals the according odds, otherwise 0.
    payout <- sum(ifelse(do_bet & is_win, best_odds, 0))
    
    # Fill results data frame.
    res$n_bets[i] <- n_bets
    res$total_stake[i] <- stake
    res$total_payout[i] <- payout
    res$net_profit[i] <- payout - stake
    res$roi[i] <- if (stake > 0) (payout - stake) / stake else NA_real_
    res$return_factor[i] <- if (stake > 0) payout / stake else NA_real_
  }
  
  res
}

# =========================================================
# Strategy 2b: MULTI BET (bet on all outcomes with EV > tau)
# =========================================================
strategy2_curve_multi_bet <- function(probs, y_true, odds_df, taus) {
  # Get the rows which can be evaluated and filter probs, y_true and odds_df accordingly.
  keep <- valid_betting_rows(y_true, odds_df)
  probs <- probs[keep, , drop = FALSE]
  y_true <- y_true[keep]
  odds_df <- odds_df[keep, , drop = FALSE]
  
  # Create a matrix with the odds.
  odds_mat <- cbind(H = odds_df$B365H, D = odds_df$B365D, A = odds_df$B365A)
  # Calculate the expected value for all possible bets.
  ev <- probs * odds_mat - 1
  # Transform outcomes to character.
  y_char <- as.character(y_true)
  
  # Initialize a results data frame.
  res <- data.frame(
    tau = taus,
    n_bets = integer(length(taus)),
    total_stake = numeric(length(taus)),
    total_payout = numeric(length(taus)),
    net_profit = numeric(length(taus)),
    roi = numeric(length(taus)),
    return_factor = numeric(length(taus))
  )
  
  # Iterate over all taus [0;4] and calculate the returns for each tau.
  for (i in seq_along(taus)) {
    tau <- taus[i]
    # Check for each match for all possible outcomes if the expected value is
    # greater than tau and create vectors who tell, on which games which bets were
    # placed.
    betH <- ev[, "H"] > tau
    betD <- ev[, "D"] > tau
    betA <- ev[, "A"] > tau
    
    n_bets <- sum(betH) + sum(betD) + sum(betA)
    stake <- n_bets
    
    # Calculate the payout.
    payout <- 0
    payout <- payout + sum(odds_mat[betH & (y_char == "H"), "H"])
    payout <- payout + sum(odds_mat[betD & (y_char == "D"), "D"])
    payout <- payout + sum(odds_mat[betA & (y_char == "A"), "A"])
    
    # Fill results data frame.
    res$n_bets[i] <- n_bets
    res$total_stake[i] <- stake
    res$total_payout[i] <- payout
    res$net_profit[i] <- payout - stake
    res$roi[i] <- if (stake > 0) (payout - stake) / stake else NA_real_
    res$return_factor[i] <- if (stake > 0) payout / stake else NA_real_
  }
  
  res
}

# =========================================================
# Strategy 2 summary helpers
# Retrieve the best row for a specific model and a specific
# strategy 2 variant.
# Criteria: highest net_profit (and lowest tau in second instance)
# =========================================================
best_tau_row <- function(df, model_name, variant_name) {
  # Only keep rows where the model name and variant fit.
  sub <- df[df$model_name == model_name & df$variant == variant_name, ]
  # If it could not be found return NULL.
  if (nrow(sub) == 0) return(NULL)
  
  # Find maximum profit.
  maxp <- max(sub$net_profit, na.rm = TRUE)
  # Retrieve all rows with the maximum profit.
  sub2 <- sub[sub$net_profit == maxp, ]
  # Order these rows with lowest tau first.
  sub2 <- sub2[order(sub2$tau), ]
  # Return first row (highest profit with lowest tau).
  sub2[1, ]
}

# =========================================================
# Bet-level net profits for Strategy 2 (for CI in profit plots)
# =========================================================
# Only one bet per game.
profits_one_bet <- function(probs, y_true, odds_df, tau) {
  # Get the rows which can be evaluated and filter probs, y_true and odds_df accordingly.
  keep <- valid_betting_rows(y_true, odds_df)
  probs <- probs[keep, , drop = FALSE]
  y_true <- y_true[keep]
  odds_df <- odds_df[keep, , drop = FALSE]
  
  # Create a matrix with the odds.
  odds_mat <- cbind(H = odds_df$B365H, D = odds_df$B365D, A = odds_df$B365A)
  # Calculate the expected value for all possible bets.
  ev <- probs * odds_mat - 1
  
  # Get the index of the highest expected value.
  best_idx <- max.col(ev, ties.method = "first")
  # Get the according outcome class (H,D,A).
  best_outcome <- colnames(ev)[best_idx]
  # Get the highest expected value per match.
  best_ev <- ev[cbind(seq_len(nrow(ev)), best_idx)]
  # Get the odds of the outcome with the highest expected value for each match.
  best_odds <- odds_mat[cbind(seq_len(nrow(odds_mat)), match(best_outcome, colnames(odds_mat)))]
  
  # Place a bet if the expected value is greater than tau.
  do_bet <- best_ev > tau
  # If no bets are place return 0.
  if (!any(do_bet)) return(numeric(0))
  
  # Transform outcomes to character and check if the outcome with the highest
  # expected value bet would be a win.
  y_char <- as.character(y_true)
  is_win <- best_outcome == y_char
  
  # Payout equals odds if bet is won, 0 otherwise.
  payout <- ifelse(is_win[do_bet], best_odds[do_bet], 0)
  # On bet-level the profit is payout - 1, because the stake is always 1.
  payout - 1
}

# Multiple bets per game.
profits_multi_bet <- function(probs, y_true, odds_df, tau) {
  # Get the rows which can be evaluated and filter probs, y_true and odds_df accordingly.
  keep <- valid_betting_rows(y_true, odds_df)
  probs <- probs[keep, , drop = FALSE]
  y_true <- y_true[keep]
  odds_df <- odds_df[keep, , drop = FALSE]
  
  # Create a matrix with the odds.
  odds_mat <- cbind(H = odds_df$B365H, D = odds_df$B365D, A = odds_df$B365A)
  # Calculate the expected value for all possible bets.
  ev <- probs * odds_mat - 1
  # Transform outcomes to character.
  y_char <- as.character(y_true)
  
  # Check for each match for all possible outcomes if the expected value is
  # greater than tau and create vectors who tell, on which games which bets were
  # placed.
  betH <- ev[, "H"] > tau
  betD <- ev[, "D"] > tau
  betA <- ev[, "A"] > tau
  
  # Check the profits of all placed bets and collect them in the vector "out".
  out <- numeric(0)
  if (any(betH)) {
    payoutH <- ifelse(y_char[betH] == "H", odds_mat[betH, "H"], 0)
    out <- c(out, payoutH - 1)
  }
  if (any(betD)) {
    payoutD <- ifelse(y_char[betD] == "D", odds_mat[betD, "D"], 0)
    out <- c(out, payoutD - 1)
  }
  if (any(betA)) {
    payoutA <- ifelse(y_char[betA] == "A", odds_mat[betA, "A"], 0)
    out <- c(out, payoutA - 1)
  }
  out
}

# Create the  profit curve over the different values of tau by delivering the 
# the number of bets, profit and 95% confidence interval borders.
strategy2_profit_ci_curve <- function(probs, y_true, odds_df, taus, variant = c("one_bet", "multi_bet")) {
  # Get the variant of the betting strategy (only "one_bet" and "multi_bet" allowed)
  variant <- match.arg(variant)
  
  # Initialize results data frame.
  res <- data.frame(
    tau = taus,
    n_bets = integer(length(taus)),
    net_profit = numeric(length(taus)),
    ci_lower = numeric(length(taus)),
    ci_upper = numeric(length(taus))
  )
  
  # Calculate the profits for all taus by calling the above defined functions.
  for (i in seq_along(taus)) {
    tau <- taus[i]
    profits <- if (variant == "one_bet") {
      profits_one_bet(probs, y_true, odds_df, tau)
    } else {
      profits_multi_bet(probs, y_true, odds_df, tau)
    }
    
    n <- length(profits)
    # Fill the results data frame.
    res$n_bets[i] <- n
    
    # No bets placed.
    if (n == 0) {
      res$net_profit[i] <- 0
      res$ci_lower[i] <- NA_real_
      res$ci_upper[i] <- NA_real_
      next
    }
    
    # Fill the results data frame.
    m <- mean(profits)
    res$net_profit[i] <- n * m
    
    # Calculate confidence interval borders and fill results data frame.
    if (n >= 2) {
      s <- sd(profits)
      se <- s / sqrt(n)
      tcrit <- qt(0.975, df = n - 1)
      lo_mean <- m - tcrit * se
      hi_mean <- m + tcrit * se
      res$ci_lower[i] <- n * lo_mean
      res$ci_upper[i] <- n * hi_mean
    } else {
      res$ci_lower[i] <- NA_real_
      res$ci_upper[i] <- NA_real_
    }
  }
  
  res
}

# =========================================================
# MAIN EXECUTION
# =========================================================

# Retrieve master odds and data-sets
master_odds_df <- build_master_odds_lookup("FootballUK_data_(+)BettingQuotes.csv")
pairs <- get_dataset_pairs()

# Retrieve model files.
model_files <- list.files("models", pattern = "\\.rds$", full.names = TRUE)
# Sanity check: Could models be found?
if (length(model_files) == 0) stop("No .rds models found in ./models/")

# Set taus from 0-4 in steps of 0.01.
taus <- seq(0, 4, by = 0.01)

# Initialize results data frames.
strategy1_results_models <- data.frame()
strategy2_onebet_all <- data.frame()
strategy2_multibet_all <- data.frame()

# Create environment to remember for which datasets Bookie-strategy 1 has already
# been done, to omit calculating it over and over again.
bookies_s1_done <- new.env(parent = emptyenv())
# Initialize results data frames.
strategy1_results_bookies <- data.frame()

# Iterate over all model files.
for (mf in model_files) {
  model_name <- tools::file_path_sans_ext(basename(mf)) # get model name
  bundle <- readRDS(mf) # loaded object + meta data
  suffix <- bundle$suffix # suffix tells the fitting data-set
  
  # Get the path to the test files of the current suffix +
  # Sanity check: Does the path exist?
  test_path <- find_test_path_by_suffix(suffix, pairs)
  if (is.na(test_path) || !file.exists(test_path)) {
    warning("Skipping (test file not found): ", model_name, " | suffix=", suffix)
    next
  }
  
  # Evaluate strategy 1 for the bookies (only once per data-set/suffix)
  if (is.null(bookies_s1_done[[suffix]])) {
    bprep <- prepare_test_for_bookies(test_path, master_odds_df) # load test data, prepare y_test and odds
    b1 <- strategy1_backtest_bookies(bprep$y_test, bprep$odds) # evaluate strategy
    b1$dataset_suffix <- suffix # add meta data
    b1$test_file <- test_path # add meta data
    strategy1_results_bookies <- rbind(strategy1_results_bookies, b1) # add results
    bookies_s1_done[[suffix]] <- TRUE # mark the data-set/suffix as done
  }
  
  # Prepare test data for model and predict probabilities.
  prep <- prepare_test_for_model(test_path, bundle, master_odds_df)
  probs <- predict_probs_from_bundle(bundle, prep$x_test)
  
  # Evaluate strategy 1 for the model and safe results.
  s1 <- strategy1_backtest_model(probs, prep$y_test, prep$odds)
  s1$model_name <- model_name
  s1$model_type <- bundle$model_type
  s1$dataset_suffix <- suffix
  s1$test_file <- test_path
  strategy1_results_models <- rbind(strategy1_results_models, s1)
  
  # Evaluate strategy 2 (one_bet) for the model and safe results.
  c1 <- strategy2_curve_one_bet(probs, prep$y_test, prep$odds, taus)
  c1$model_name <- model_name
  c1$model_type <- bundle$model_type
  c1$dataset_suffix <- suffix
  c1$test_file <- test_path
  strategy2_onebet_all <- rbind(strategy2_onebet_all, c1)
  
  # Evaluate strategy 2 (multi_bet) for the model and safe results.
  c2 <- strategy2_curve_multi_bet(probs, prep$y_test, prep$odds, taus)
  c2$model_name <- model_name
  c2$model_type <- bundle$model_type
  c2$dataset_suffix <- suffix
  c2$test_file <- test_path
  strategy2_multibet_all <- rbind(strategy2_multibet_all, c2)
  
  cat("Processed model: ", model_name, "\n", sep = "")
}

#write.csv(strategy1_results_models, "Strategy1_Profit_AllModels.csv", row.names = FALSE)
#write.csv(strategy1_results_bookies, "Strategy1_Profit_Bookies.csv", row.names = FALSE)

cat("\nSaved: Strategy1_Profit_AllModels.csv\n")
cat("Saved: Strategy1_Profit_Bookies.csv\n")

#write.csv(strategy2_onebet_all, "Strategy2_OneBet_AllModels.csv", row.names = FALSE)
#write.csv(strategy2_multibet_all, "Strategy2_MultiBet_AllModels.csv", row.names = FALSE)

cat("Saved: Strategy2_OneBet_AllModels.csv\n")
cat("Saved: Strategy2_MultiBet_AllModels.csv\n")

# =========================================================
# Determine best models for Strategy 2 (across both variants)
# =========================================================
# Combine the results from both variants of strategy 2 to one big table.
s2_combined <- rbind(
  transform(strategy2_onebet_all, variant = "one_bet"),
  transform(strategy2_multibet_all, variant = "multi_bet")
)

# Group by model name and find the maximum profit in each group.
max_profit_by_model <- aggregate(net_profit ~ model_name, data = s2_combined, FUN = max, na.rm = TRUE)
# Find the one model with the highest profit.
best_max_profit_model <- max_profit_by_model$model_name[which.max(max_profit_by_model$net_profit)]

# Count for how many taus each model returned a positive profit.
pos_count_by_model <- aggregate(net_profit ~ model_name, data = s2_combined,
                                FUN = function(x) sum(x > 0, na.rm = TRUE))
# Find the model with the most positive returns (profit).
best_poscount_model <- pos_count_by_model$model_name[which.max(pos_count_by_model$net_profit)]

# Names of the before found models.
models_to_report <- unique(c(best_max_profit_model, best_poscount_model))

# Create summary data frame.
summary_best_models <- data.frame(
  best_max_profit_model = best_max_profit_model,
  best_poscount_model = best_poscount_model,
  stringsAsFactors = FALSE
)
#write.csv(summary_best_models, "Strategy2_Summary_BestModels_OnlyNames.csv", row.names = FALSE)
cat("\nSaved: Strategy2_Summary_BestModels_OnlyNames.csv\n")

# =========================================================
# Extended summary for the two selected models
# =========================================================
rows <- list()

# Iterate over all models and variants in models_to_report
for (m in models_to_report) {
  for (v in c("one_bet", "multi_bet")) {
    # Find the row (model) with maximum net profit
    br <- best_tau_row(s2_combined, m, v)
    if (is.null(br)) next
    
    # Add the model to the list.
    rows[[length(rows) + 1]] <- data.frame(
      selection_reason = if (m == best_max_profit_model) "highest_max_profit" else "most_positive_taus",
      model_name = m,
      variant = v,
      best_tau = br$tau,
      n_bets = br$n_bets,
      total_stake = br$total_stake,
      total_payout = br$total_payout,
      net_profit = br$net_profit,
      roi = br$roi,
      return_factor = br$return_factor,
      stringsAsFactors = FALSE
    )
  }
}

# Bind list with results to a data frame.
summary_best_extended <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
#write.csv(summary_best_extended, "Strategy2_Summary_BestModels.csv", row.names = FALSE)
cat("Saved: Strategy2_Summary_BestModels.csv\n")
if (nrow(summary_best_extended) > 0) print(summary_best_extended)

# =========================================================
# Plot profit vs tau for the two selected models (both variants)
# with 95% CI bands for Net Profit
# =========================================================
# Iterate over the chosen models
for (mname in models_to_report) {
  
  # Sanity check: Is the model file available?
  mf_path <- file.path("models", paste0(mname, ".rds"))
  if (!file.exists(mf_path)) {
    warning("Model file not found for plotting CI: ", mf_path)
    next
  }
  # Read in the model bundle.
  bundle <- readRDS(mf_path)
  
  # Sanity check: Is the test file available?
  test_path <- find_test_path_by_suffix(bundle$suffix, pairs)
  if (is.na(test_path) || !file.exists(test_path)) {
    warning("Test file not found for model CI plotting: ", mname, " | suffix=", bundle$suffix)
    next
  }
  
  # Prepare test data.
  prep <- prepare_test_for_model(test_path, bundle, master_odds_df)
  # Get prediction probabilities.
  probs <- predict_probs_from_bundle(bundle, prep$x_test)
  
  # One-bet curve plot with CI
  # Check if the required results are there.
  if (nrow(strategy2_onebet_all[strategy2_onebet_all$model_name == mname, ]) > 0) {
    # Collect number of bets, profit, confidence interval lower border and 
    # confidence interval upper border for each tau
    ci1 <- strategy2_profit_ci_curve(probs, prep$y_test, prep$odds, taus, variant = "one_bet")
    # Filter for NAs (there should not be any)
    plot_df <- ci1[!is.na(ci1$net_profit), ]
    if (nrow(plot_df) > 0) {
      # Create a png for the plot.
      png1 <- paste0("Strategy2_OneBet_ProfitCurve_CI_", mname, ".png")
      png(png1, width = 900, height = 600)
      
      # Set borders.
      ymin <- min(plot_df$ci_lower, plot_df$net_profit, 0, na.rm = TRUE)
      ymax <- max(plot_df$ci_upper, plot_df$net_profit, 0, na.rm = TRUE)
      
      # Plot the profit line over the different values of tau.
      plot(plot_df$tau, plot_df$net_profit, type = "l",
           main = paste0("Strategy 2a (max 1 bet/match) Profit vs tau (95% CI): ", mname),
           xlab = "tau", ylab = "Net Profit (units)",
           ylim = c(ymin, ymax))
      
      # Add confidence interval.
      # Check for NAs in the borders.
      ci_ok <- !is.na(plot_df$ci_lower) & !is.na(plot_df$ci_upper)
      # Plot the confidence interval.
      if (any(ci_ok)) {
        x <- plot_df$tau[ci_ok]
        ylo <- plot_df$ci_lower[ci_ok]
        yhi <- plot_df$ci_upper[ci_ok]
        polygon(c(x, rev(x)), c(ylo, rev(yhi)),
                border = NA,
                col = grDevices::adjustcolor("grey60", alpha.f = 0.35))
        lines(plot_df$tau, plot_df$net_profit, lwd = 2)
      }
      
      # Add break-even line, legend and shut off device.
      abline(h = 0, lty = 2)
      legend("topright",
             legend = c("net_profit", "95% CI"),
             lty = c(1, NA),
             lwd = c(2, NA),
             pch = c(NA, 15),
             pt.cex = c(NA, 2),
             bty = "n")
      
      dev.off()
      cat("Saved plot: ", png1, "\n", sep = "")
    }
  }
  
  # Multi-bet curve plot with CI
  # Check if the required results are there.
  if (nrow(strategy2_multibet_all[strategy2_multibet_all$model_name == mname, ]) > 0) {
    # Collect number of bets, profit, confidence interval lower border and 
    # confidence interval upper border for each tau
    ci2 <- strategy2_profit_ci_curve(probs, prep$y_test, prep$odds, taus, variant = "multi_bet")
    # Filter for NAs (there should not be any)
    plot_df <- ci2[!is.na(ci2$net_profit), ]
    if (nrow(plot_df) > 0) {
      # Create a png for the plot.
      png2 <- paste0("Strategy2_MultiBet_ProfitCurve_CI_", mname, ".png")
      png(png2, width = 900, height = 600)
      
      # Set borders.
      ymin <- min(plot_df$ci_lower, plot_df$net_profit, 0, na.rm = TRUE)
      ymax <- max(plot_df$ci_upper, plot_df$net_profit, 0, na.rm = TRUE)
      
      # Plot the profit line over the different values of tau.
      plot(plot_df$tau, plot_df$net_profit, type = "l",
           main = paste0("Strategy 2b (bet all EV>tau) Profit vs tau (95% CI): ", mname),
           xlab = "tau", ylab = "Net Profit (units)",
           ylim = c(ymin, ymax))
      
      # Add confidence interval.
      # Check for NAs in the borders.
      ci_ok <- !is.na(plot_df$ci_lower) & !is.na(plot_df$ci_upper)
      # Plot the confidence interval.
      if (any(ci_ok)) {
        x <- plot_df$tau[ci_ok]
        ylo <- plot_df$ci_lower[ci_ok]
        yhi <- plot_df$ci_upper[ci_ok]
        polygon(c(x, rev(x)), c(ylo, rev(yhi)),
                border = NA,
                col = grDevices::adjustcolor("grey60", alpha.f = 0.35))
        lines(plot_df$tau, plot_df$net_profit, lwd = 2)
      }
      
      # Add break-even line, legend and shut off device.
      abline(h = 0, lty = 2)
      legend("topright",
             legend = c("net_profit", "95% CI"),
             lty = c(1, NA),
             lwd = c(2, NA),
             pch = c(NA, 15),
             pt.cex = c(NA, 2),
             bty = "n")
      
      dev.off()
      cat("Saved plot: ", png2, "\n", sep = "")
    }
  }
}

cat("\nDone.\n")
