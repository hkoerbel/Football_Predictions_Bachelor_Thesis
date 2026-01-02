# What this script is for:
# Determine best models by return factor leveraging betting strategy 2
# + plot return factor vs tau with 95% confidence intervals
#
# Search for:
#   (A) Highest return_factor overall
#   (B) Highest return_factor with 95% CI lower bound > 1
#   (C) Highest return_factor with at least 30 bets placed (n_bets >= 30)
#
# Requires:
#   - Strategy2_OneBet_AllModels.csv
#   - Strategy2_MultiBet_AllModels.csv
#   - models/<model_name>.rds
#   - FootballUK_data_(+)BettingQuotes.csv
#   - test data folders
#
# Outputs:
#   - Strategy2_Summary_BestModels_ReturnFactor_AllCriteria.csv
#   - Strategy2_ReturnFactorCurve_CI_<reason>_<model>_<variant>.png
#
# Most parts of this script are a copy from "V3_11_Evaluate_Betting_Strategies_1.R".
# It was reused, because the decision to analyze the return factor was made after 
# already running "V3_11_Evaluate_Betting_Strategies_1.R" and therefore creating 
# various data files and obtaining model results. To avoid overwriting the results
# of the models, this new script was created. Additionally, this script leverages
# the files created in "V3_11_Evaluate_Betting_Strategies_1.R".
# Furthermore, with this distinction of scripts a more clear distinction of analysis
# focus was sustained.

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

# =========================================================
# Helper to read in data
# =========================================================
read_csv_safe <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

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
  out
}

# ---------------------------------------------------------
# Discover test files by suffix and create a list with them.
# (The function works the same as "get_dataset_pairs()" from
# "V3_09_Create_Models.R", therefore they received the same
# name, even though here only the test files are retrieved.)
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# Create a master odds lookup table to fill missing odds in
# test data (should never be necessary but works as an additional
# safety approach).
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# Prepare test data for prediction + attach odds (test odds first, then master fill)
# ---------------------------------------------------------
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
  x_test <- apply_imputer(x_test, bundle$imputer)
  
  # Return list with explanatory variables, target variable and odds.
  list(x_test = x_test, y_test = y_test, odds = odds)
}

# ---------------------------------------------------------
# Predict probabilities from saved model bundle
# ---------------------------------------------------------
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
    raw <- predict(bundle$model, xgboost::xgb.DMatrix(mm_aligned))
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

# ---------------------------------------------------------
# Betting row validity check
# Filter for which match rows bets can be evaluated (they all should fit).
# ---------------------------------------------------------
valid_betting_rows <- function(y_true, odds_df) {
  # The true result is needed.
  ok_outcome <- !is.na(y_true)
  # Odds cannot be NA and have to be greater than 0.
  ok_odds <- !is.na(odds_df$B365H) & !is.na(odds_df$B365D) & !is.na(odds_df$B365A) &
    odds_df$B365H > 0 & odds_df$B365D > 0 & odds_df$B365A > 0
  ok_outcome & ok_odds
}

# ---------------------------------------------------------
# Bet-level payouts for Strategy 2 variants (for CI)
# ---------------------------------------------------------
# ONE BET
payouts_one_bet <- function(probs, y_true, odds_df, tau) {
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
  # If no bet is placed, return 0.
  if (!any(do_bet)) return(numeric(0))
  
  # Transform outcomes to character and check if the outcome with the highest
  # expected value bet would be a win.
  y_char <- as.character(y_true)
  is_win <- best_outcome == y_char
  
  # Payout equals odds if bet is won, 0 otherwise.
  ifelse(is_win[do_bet], best_odds[do_bet], 0)
}

# MULTI BET
payouts_multi_bet <- function(probs, y_true, odds_df, tau) {
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
  
  # Check the payouts of all placed bets and collect them in the vector "out".
  out <- numeric(0)
  if (any(betH)) out <- c(out, ifelse(y_char[betH] == "H", odds_mat[betH, "H"], 0))
  if (any(betD)) out <- c(out, ifelse(y_char[betD] == "D", odds_mat[betD, "D"], 0))
  if (any(betA)) out <- c(out, ifelse(y_char[betA] == "A", odds_mat[betA, "A"], 0))
  out
}

# Create the framework for a return factor curve over the different values of tau by delivering the 
# the number of bets, return factor and 95% confidence interval borders.
return_factor_ci_at_tau <- function(probs, y_true, odds_df, tau, variant = c("one_bet", "multi_bet")) {
  # Get the variant of the betting strategy (only "one_bet" and "multi_bet" allowed)
  variant <- match.arg(variant)
  
  #  Calculate the payouts by calling the above defined functions.
  payouts <- if (variant == "one_bet") {
    payouts_one_bet(probs, y_true, odds_df, tau)
  } else {
    payouts_multi_bet(probs, y_true, odds_df, tau)
  }
  
  n <- length(payouts)
  
  # No bets placed.
  if (n == 0) {
    return(list(n_bets = 0L, return_factor = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
  }
  
  # Calculate confidence interval borders.
  m <- mean(payouts)
  if (n >= 2) {
    s <- sd(payouts)
    se <- s / sqrt(n)
    tcrit <- qt(0.975, df = n - 1)
    lo <- m - tcrit * se
    hi <- m + tcrit * se
  } else {
    lo <- NA_real_
    hi <- NA_real_
  }
  
  # Fill the results list.
  list(n_bets = as.integer(n), return_factor = m, ci_lower = lo, ci_upper = hi)
}

# Create a return factor curve over the different tau values.
curve_return_factor_ci <- function(probs, y_true, odds_df, taus, variant = c("one_bet", "multi_bet")) {
  # Get the variant of the betting strategy (only "one_bet" and "multi_bet" allowed)
  variant <- match.arg(variant)
  
  # Initialize vectors for results.
  rf <- rep(NA_real_, length(taus))
  lo <- rep(NA_real_, length(taus))
  hi <- rep(NA_real_, length(taus))
  nb <- rep(0L, length(taus))
  
  # Iterate over all taus and calculate the variables of interest for all taus
  # by calling the above defined function return_factor_ci_at_tau().
  for (i in seq_along(taus)) {
    tau <- taus[i]
    tmp <- return_factor_ci_at_tau(probs, y_true, odds_df, tau, variant)
    nb[i] <- tmp$n_bets
    rf[i] <- tmp$return_factor
    lo[i] <- tmp$ci_lower
    hi[i] <- tmp$ci_upper
  }
  
  # Return results as data frame.
  data.frame(tau = taus, n_bets = nb, return_factor = rf, ci_lower = lo, ci_upper = hi)
}

# Helper for naming files.
safe_name <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

# Plot the curve with confidence intervals.
plot_curve_ci <- function(curve_ci, model_name, variant, best_tau, reason_tag) {
  # Get rid off rows with return_factor == NA
  plot_df <- curve_ci[!is.na(curve_ci$return_factor), ]
  # Sanity check: Are there points to plot?
  if (nrow(plot_df) == 0) {
    cat("No points to plot for", model_name, variant, "\n")
    return(invisible(NULL))
  }
  
  # Create a png for the plot.
  png_file <- paste0("Strategy2_ReturnFactorCurve_CI_", reason_tag, "_", safe_name(model_name), "_", variant, ".png")
  png(png_file, width = 1000, height = 650)
  
  # Set borders.
  ymin <- min(plot_df$ci_lower, plot_df$return_factor, 0, na.rm = TRUE)
  ymax <- max(plot_df$ci_upper, plot_df$return_factor, 0, na.rm = TRUE)
  
  # Plot the profit line over the different values of tau.
  plot(plot_df$tau, plot_df$return_factor, type = "l",
       main = paste0("Strategy 2 (", variant, ") Return Factor vs tau: ", model_name, " [", reason_tag, "]"),
       xlab = "tau",
       ylab = "Return factor (total_payout / total_stake)",
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
    lines(plot_df$tau, plot_df$return_factor, lwd = 2)
  }
  
  # Add break-even line, marker for best tau, legend and shut off device.
  abline(h = 1, lty = 2)
  abline(v = best_tau, lty = 3)
  legend("topright",
         legend = c("return_factor", "95% CI", "break-even (1.0)", paste0("selected tau=", best_tau)),
         lty = c(1, NA, 2, 3),
         lwd = c(2, NA, 1, 1),
         pch = c(NA, 15, NA, NA),
         pt.cex = c(NA, 2, NA, NA),
         bty = "n")
  
  dev.off()
  cat("Saved plot: ", png_file, "\n", sep = "")
}

# =========================================================
# Load Strategy 2 curve outputs
# =========================================================
# Define file paths.
one_path <- "Strategy2_OneBet_AllModels.csv"
multi_path <- "Strategy2_MultiBet_AllModels.csv"
# Sanity check: Do the files exist?
if (!file.exists(one_path)) stop("Missing file: ", one_path)
if (!file.exists(multi_path)) stop("Missing file: ", multi_path)

# Read in files.
s2_one   <- read_csv_safe(one_path)
s2_multi <- read_csv_safe(multi_path)

# Combine the results from both variants of strategy 2 to one big table.
s2_combined <- rbind(
  transform(s2_one,   variant = "one_bet"),
  transform(s2_multi, variant = "multi_bet")
)

# Filter out rows with "normal" numbers.
s2_combined <- s2_combined[is.finite(s2_combined$return_factor) & !is.na(s2_combined$return_factor), ]
if ("total_stake" %in% names(s2_combined)) s2_combined <- s2_combined[s2_combined$total_stake > 0, ]
if (nrow(s2_combined) == 0) stop("No valid rows left after filtering.")

# Retrieve taus.
taus <- sort(unique(s2_combined$tau))

# =========================================================
# Highest return_factor overall
# =========================================================
# Group by model name and find the maximum return factor in each group.
max_rf_by_model <- aggregate(return_factor ~ model_name, data = s2_combined, FUN = max, na.rm = TRUE)
# Find the one model with the highest return factor.
best_rf_model <- max_rf_by_model$model_name[which.max(max_rf_by_model$return_factor)]

# Retrieve all rows (taus) with the best model.
subA <- s2_combined[s2_combined$model_name == best_rf_model, ]
# Retrieve the best return factor this model reaches.
best_rf <- max(subA$return_factor, na.rm = TRUE)
# Retrieve all rows with the highest return factor.
subA2 <- subA[subA$return_factor == best_rf, ]
# In case there is more than one line (tau) prefer the smallest tau leading to
# the highest return factor.
subA2 <- subA2[order(subA2$tau), ]
best_row_A <- subA2[1, ]

# =========================================================
# Highest return_factor with at least 30 bets (CSV-based)
# =========================================================
# Initialize.
best_row_C <- NULL
# Filter for candidates with at least 30 placed bets.
cand_C <- s2_combined[!is.na(s2_combined$n_bets) & s2_combined$n_bets >= 30, ]
if (nrow(cand_C) > 0) {
  # Find highest return factor in remaining rows.
  mx <- max(cand_C$return_factor, na.rm = TRUE)
  # Retrieve all columns with the maximum return factor.
  cand2 <- cand_C[cand_C$return_factor == mx, ]
  # In case there is more than one line (tau) prefer the smallest tau leading to
  # the highest return factor.
  cand2 <- cand2[order(cand2$tau), ]
  best_row_C <- cand2[1, ]
}

# =========================================================
# Load master odds + dataset mapping + caching
# =========================================================
# Retrieve master odds and data-sets
master_odds_df <- build_master_odds_lookup("FootballUK_data_(+)BettingQuotes.csv")
pairs <- get_dataset_pairs()

# Create environment to store objects with predictions, predictions are only run 
# once, but can accessed more often.
pred_cache <- new.env(parent = emptyenv())
get_model_data <- function(model_name) {
  # Check if the model is already in the cache environment and return it if yes.
  if (!is.null(pred_cache[[model_name]])) return(pred_cache[[model_name]])
  
  # Load model path
  model_path <- file.path("models", paste0(model_name, ".rds"))
  # Sanity check: Does the file exist?
  if (!file.exists(model_path)) stop("Model file not found: ", model_path)
  
  # Read in model.
  bundle <- readRDS(model_path)
  # Load test file path.
  test_path <- find_test_path_by_suffix(bundle$suffix, pairs)
  # Sanity check: Does the file exist?
  if (is.na(test_path) || !file.exists(test_path)) stop("Test file not found for suffix: ", bundle$suffix)
  
  # Prepare test data.
  prep <- prepare_test_for_model(test_path, bundle, master_odds_df)
  # Predict probabilities.
  probs <- predict_probs_from_bundle(bundle, prep$x_test)
  
  # Add the current object to the cache.
  obj <- list(bundle = bundle, probs = probs, y_true = prep$y_test, odds = prep$odds)
  pred_cache[[model_name]] <- obj
  obj
}

# =========================================================
# Highest return_factor with 95% CI lower bound > 1
# =========================================================
best_row_B <- NULL
# Filter for candidates with at least 2 placed bets (necessary for confidence intervals).
cand_B <- s2_combined[!is.na(s2_combined$n_bets) & s2_combined$n_bets >= 2, ]
# Sort by highest return factor and smallest tau.
cand_B <- cand_B[order(-cand_B$return_factor, cand_B$tau), ]

if (nrow(cand_B) > 0) {
  # Get the confidence interval for all models still considered.
  for (i in seq_len(nrow(cand_B))) {
    r <- cand_B[i, ]
    md <- get_model_data(r$model_name)
    ci <- return_factor_ci_at_tau(md$probs, md$y_true, md$odds, tau = r$tau, variant = r$variant)
    
    # Check if a model fulfills the requirement of having a lower bound of
    # the confidence interval greater than 1.
    if (is.finite(ci$ci_lower) && !is.na(ci$ci_lower) && ci$ci_lower > 1) {
      best_row_B <- r
      best_row_B$return_factor_ci <- ci$return_factor
      best_row_B$ci_lower <- ci$ci_lower
      best_row_B$ci_upper <- ci$ci_upper
      best_row_B$n_bets_ci <- ci$n_bets
      break
    }
  }
}

# =========================================================
# Build one combined summary CSV (all criteria)
# =========================================================
make_summary_row <- function(tag, row_obj) {
  if (is.null(row_obj) || is.na(row_obj$model_name) || is.na(row_obj$variant) || is.na(row_obj$tau)) {
    return(data.frame(
      selection_reason = tag,
      model_name = NA_character_,
      variant = NA_character_,
      best_tau = NA_real_,
      n_bets = NA_integer_,
      total_stake = NA_real_,
      total_payout = NA_real_,
      net_profit = NA_real_,
      roi = NA_real_,
      return_factor = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  # If CI fields exist use them; else NA
  ci_lo <- if ("ci_lower" %in% names(row_obj)) row_obj$ci_lower else NA_real_
  ci_hi <- if ("ci_upper" %in% names(row_obj)) row_obj$ci_upper else NA_real_
  rf    <- if ("return_factor_ci" %in% names(row_obj)) row_obj$return_factor_ci else row_obj$return_factor
  
  data.frame(
    selection_reason = tag,
    model_name = row_obj$model_name,
    variant = row_obj$variant,
    best_tau = row_obj$tau,
    n_bets = row_obj$n_bets,
    total_stake = row_obj$total_stake,
    total_payout = row_obj$total_payout,
    net_profit = row_obj$net_profit,
    roi = row_obj$roi,
    return_factor = rf,
    ci_lower = ci_lo,
    ci_upper = ci_hi,
    stringsAsFactors = FALSE
  )
}

summary_all <- rbind(
  make_summary_row("highest_return_factor", best_row_A),
  make_summary_row("highest_return_factor_ci_lower_gt_1", best_row_B),
  make_summary_row("highest_return_factor_min30bets", best_row_C)
)

#write.csv(summary_all, "Strategy2_Summary_BestModels_ReturnFactor_AllCriteria.csv", row.names = FALSE)

cat("\nCombined summary saved: Strategy2_Summary_BestModels_ReturnFactor_AllCriteria.csv\n")
print(summary_all)

# =========================================================
# Plot curves for the found selections (avoid duplicates)
# =========================================================
# Bundle best rows in a list.
rows_to_plot <- list(
  best_overall    = best_row_A,
  best_CIpositive = best_row_B,
  best_min30bets  = best_row_C
)

# List to remember which combinations have already been plotted.
seen <- character(0)

# Iterate over the models/rows to plot.
for (reason_tag in names(rows_to_plot)) {
  r <- rows_to_plot[[reason_tag]]
  # If something misses -> skip.
  if (is.null(r) || is.na(r$model_name) || is.na(r$variant) || is.na(r$tau)) next
  
  # Create a unique key to identify the models.
  key <- paste(r$model_name, r$variant, sep = "___")
  # If a model has already been plotted -> skip; Otherwise add it to "seen"
  if (key %in% seen) next
  seen <- c(seen, key)
  
  # Load the data for the model.
  md <- get_model_data(r$model_name)
  # Calculate the return factor and confidence interval for the model.
  curve_ci <- curve_return_factor_ci(md$probs, md$y_true, md$odds, taus, variant = r$variant)
  # Plot the model.
  plot_curve_ci(curve_ci, r$model_name, r$variant, best_tau = r$tau, reason_tag = reason_tag)
}

cat("\nDone.\n")
