# What this script is for:
# Compute bookmaker performance on test data (Bet365 odds).

# Define file path
test_path <- file.path(
  "data_(-)FormParameter_(+)BettingQuotes",
  "all_variables_(-)form_parameter_(+)betting_quotes_test_data.csv"
)

# Sanity check: Does the file exist?
if (!file.exists(test_path)) {
  stop("File not found: ", test_path)
}

# Read in data
df <- read.csv(test_path, stringsAsFactors = FALSE, check.names = FALSE)

# Define the needed columns.
needed <- c("FTR", "B365H", "B365D", "B365A")

# Sanity check: Are all required columns available?
missing_cols <- setdiff(needed, names(df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Change FTR column to factor.
df$FTR <- factor(df$FTR, levels = c("H", "D", "A"))

# Check if each row has valid data (no NAs and odds > 0).
valid <- !is.na(df$FTR) &
  !is.na(df$B365H) & !is.na(df$B365D) & !is.na(df$B365A) &
  df$B365H > 0 & df$B365D > 0 & df$B365A > 0

# Get only the rows with valid data from df.
dat <- df[valid, c("FTR", "B365H", "B365D", "B365A")]
# Get number of matches with valid data.
M <- nrow(dat)

if (M == 0) {
  stop("No valid rows found after filtering (missing/invalid odds or outcomes).")
}

# Normalize the probabilities implied in the betting odds.
qH <- 1 / dat$B365H
qD <- 1 / dat$B365D
qA <- 1 / dat$B365A

S <- qH + qD + qA

pH <- qH / S
pD <- qD / S
pA <- qA / S

# Calculate the RPS.
yH <- as.integer(dat$FTR == "H")
yA <- as.integer(dat$FTR == "A")

rps <- (1 / (2 * M)) * sum((pH - yH)^2 + (pA - yA)^2)


# Assumption: Bookmaker prediction = lowest odds
# Deterministic tie-break: H > D > A
# Create a new matrix with the odds.
odds_mat <- cbind(H = dat$B365H, D = dat$B365D, A = dat$B365A)
# Get the indices of the lowest odds (1 -> carry out function on the rows).
pred_idx <- apply(odds_mat, 1, which.min)
# Change indices back to the prediction classes to obtain the prediction.
pred <- factor(colnames(odds_mat)[pred_idx], levels = c("H", "D", "A"))

# Calculate accuracy.
accuracy <- mean(pred == dat$FTR) * 100

# Count how often each outcome is predicted and how often they really occur.
pred_counts <- table(pred)
true_counts <- table(dat$FTR)

# Since table() only gets levels that appear at least once it would not return
# a number for predictions that do not occur. With this the function the count
# of all desired levels can be checked in case a level does not appear a "0" (integer)
# is returned.
get_count <- function(tab, lvl) if (lvl %in% names(tab)) as.integer(tab[[lvl]]) else 0L

# Collect results.
results <- data.frame(
  dataset = "av_(-)fp_(+)bq_test",
  file = test_path,
  n_matches = M,
  rps = as.numeric(rps),
  accuracy_percent = as.numeric(accuracy),
  predicted_H = get_count(pred_counts, "H"),
  predicted_D = get_count(pred_counts, "D"),
  predicted_A = get_count(pred_counts, "A"),
  true_H = get_count(true_counts, "H"),
  true_D = get_count(true_counts, "D"),
  true_A = get_count(true_counts, "A"),
  stringsAsFactors = FALSE
)

# Safe results.
output_file <- "Bookies_Performance.csv"
#write.csv(results, output_file, row.names = FALSE)

# Print summary
cat("\n================ Bookmaker Evaluation (Bet365 odds) ================\n")
cat("Saved results to:", output_file, "\n\n")
print(results)
cat("====================================================================\n\n")