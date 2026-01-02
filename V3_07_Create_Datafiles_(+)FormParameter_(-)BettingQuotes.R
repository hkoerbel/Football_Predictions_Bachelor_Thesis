# What this script is for:
# Merge data sets holding all game specific variables with Form Parameter versions.
#
# 1) Reads the base datasets:
#    - data_(-)FormParameter_(-)BettingQuotes/
#      * all_variables_(-)form_parameter_(-)betting_quotes_train_data.csv
#      * all_variables_(-)form_parameter_(-)betting_quotes_test_data.csv
#
# 2) Iterates over all form-parameter file pairs in:
#    - data_onlyFormParameter/
#      * gamma_XXX_train.csv
#      * gamma_XXX_test.csv
# (XXX represents the gamma value used to obtain the Form Parameter.)
#
# 3) For each gamma version, it LEFT-JOINS Home_Form / Away_Form into the base
#    train/test datasets using the keys: HomeTeam, AwayTeam, Date
#
# 4) Writes new files to:
#    - data_(+)FormParameter_(-)Betting Quotes/
#      * all_variables_(+)form_parameter_XXX_(-)betting_quotes_train_data.csv
#      * all_variables_(+)form_parameter_XXX_(-)betting_quotes_test_data.csv
# (XXX represents the gamma value used to obtain the Form Parameter.)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

#Define file paths in the beginning.
data_root <- "."

# Folder with the form-parameter-only files (gamma_XXX_train/test.csv)
form_dir <- file.path(data_root, "data_onlyFormParameter")

# Folder with the base "all_variables ..." train/test files
base_dir <- file.path(data_root, "data_(-)FormParameter_(-)BettingQuotes")

base_train_file <- file.path(
  base_dir,
  "all_variables_(-)form_parameter_(-)betting_quotes_train_data.csv"
)
base_test_file <- file.path(
  base_dir,
  "all_variables_(-)form_parameter_(-)betting_quotes_test_data.csv"
)

# Output folder for the merged datasets
out_dir <- file.path(data_root, "data_(+)FormParameter_(-)BettingQuotes")

# Create output directory if it does not exist
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Helper for date parsing.
# A small set of common formats is tried. If parsing fails, it falls back to the
# trimmed original string as the join key (still works if both files match 
# exactly, which they should).
parse_date_try <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- trimws(as.character(x))

  # Try common formats
  d <- as.Date(
    x_chr,
    tryFormats = c(
      "%d.%m.%Y",  # 19.08.2002
      "%d/%m/%Y",  # 19/08/2002
      "%d/%m/%y",  # 19/08/02
      "%Y-%m-%d",  # 2002-08-19
      "%d.%m.%y"   # 19.08.02
    )
  )
  d
}

# Helper to normalize join keys.
# Build a safe join key:
# - HomeTeam / AwayTeam: trimmed character
# - Date_key: if parsable -> date string, else trimmed original (using the above helper)
add_join_keys <- function(df) {
  # Sanity check to see if all required columns are available
  need <- c("HomeTeam", "AwayTeam", "Date")
  if (!all(need %in% names(df))) {
    stop("Missing required columns: ", paste(setdiff(need, names(df)), collapse = ", "))
  }

  # Normalize team names (handle factors, extra spaces)
  df <- df %>%
    mutate(
      HomeTeam = trimws(as.character(HomeTeam)),
      AwayTeam = trimws(as.character(AwayTeam))
    )

  # Create a robust date key
  # Try parsing to date format.
  d_parsed <- parse_date_try(df$Date)
  # If parsing to date format worked -> use "%Y-%m-%d" format. If it did not 
  # work -> trim white spaces and use the Date column in characters.
  date_key <- ifelse(!is.na(d_parsed), format(d_parsed, "%Y-%m-%d"), trimws(as.character(df$Date)))

  df$Date_key <- date_key
  df
}


# Check if base datasets exist
if (!file.exists(base_train_file)) stop("Base train file not found: ", base_train_file)
if (!file.exists(base_test_file))  stop("Base test file not found: ", base_test_file)

# Read base datasets
base_train <- readr::read_csv(base_train_file, show_col_types = FALSE)
base_test  <- readr::read_csv(base_test_file,  show_col_types = FALSE)

# Add join keys to base datasets
base_train_k <- add_join_keys(base_train)
base_test_k  <- add_join_keys(base_test)


# Discover form-parameter file pairs.
# Check if the required directory exists.
if (!dir.exists(form_dir)) stop("Form directory not found: ", form_dir)

# Start with the train files.
train_form_files <- list.files(
  path = form_dir,
  pattern = "^gamma_[0-9]{3}_train\\.csv$", #files are named "gamma_XXX_train.csv", XXX representing the used gamma value
  full.names = TRUE
)

# Check if files have been found.
if (length(train_form_files) == 0) {
  stop("No form train files found in: ", form_dir, " (expected pattern: gamma_XXX_train.csv)")
}

# Now get the test files.
for (train_path in train_form_files) {

  # basename only returns the filename from the filepath (in this case the name of the train file)
  train_name <- basename(train_path)
  # Extract the XXX for the gamma value from the train file name.
  gamma_id <- str_match(train_name, "^gamma_([0-9]{3})_train\\.csv$")[, 2]

  # Build expected test file path
  test_path <- file.path(form_dir, paste0("gamma_", gamma_id, "_test.csv"))

  # Sanity check to see if the test file exists.
  if (!file.exists(test_path)) {
    warning("Skipping gamma_", gamma_id, " because test file is missing: ", test_path)
    next
  }

  message("Merging gamma_", gamma_id, " ...")

  # -------------------------
  # Read form-parameter data for this gamma
  # -------------------------
  form_train <- readr::read_csv(train_path, show_col_types = FALSE)
  form_test  <- readr::read_csv(test_path,  show_col_types = FALSE)

  # Ensure the required columns exist in the form files
  needed_form_cols <- c("HomeTeam", "AwayTeam", "Date", "Home_Form", "Away_Form")
  if (!all(needed_form_cols %in% names(form_train))) {
    stop("Missing required columns in ", train_name, ": ",
         paste(setdiff(needed_form_cols, names(form_train)), collapse = ", "))
  }
  if (!all(needed_form_cols %in% names(form_test))) {
    stop("Missing required columns in ", basename(test_path), ": ",
         paste(setdiff(needed_form_cols, names(form_test)), collapse = ", "))
  }

  # Add join keys
  form_train_k <- add_join_keys(form_train)
  form_test_k  <- add_join_keys(form_test)

  # Keep only the join keys + the two form columns for merging
  form_train_small <- form_train_k %>%
    select(HomeTeam, AwayTeam, Date_key, Home_Form, Away_Form)

  form_test_small <- form_test_k %>%
    select(HomeTeam, AwayTeam, Date_key, Home_Form, Away_Form)

  # -------------------------
  # Sanity check: duplicates in form files (can break joins; should not appear)
  # -------------------------
  # If duplicates exist for the join key, the join would create duplicated rows.
  # Write a warning, but still proceed for now.
  dup_train <- form_train_small %>%
    count(HomeTeam, AwayTeam, Date_key) %>%
    filter(n > 1)
  if (nrow(dup_train) > 0) {
    warning("Duplicate join keys found in form TRAIN file for gamma_", gamma_id,
            ". Join may duplicate rows. Example count: ", dup_train$n[1])
  }

  dup_test <- form_test_small %>%
    count(HomeTeam, AwayTeam, Date_key) %>%
    filter(n > 1)
  if (nrow(dup_test) > 0) {
    warning("Duplicate join keys found in form TEST file for gamma_", gamma_id,
            ". Join may duplicate rows. Example count: ", dup_test$n[1])
  }

  # -------------------------
  # Merge into base data (LEFT JOIN keeps base rows/order)
  # -------------------------
  merged_train <- base_train_k %>%
    left_join(form_train_small, by = c("HomeTeam", "AwayTeam", "Date_key"))

  merged_test <- base_test_k %>%
    left_join(form_test_small, by = c("HomeTeam", "AwayTeam", "Date_key"))

  # Remove internal join key column before saving
  merged_train <- merged_train %>% select(-Date_key)
  merged_test  <- merged_test  %>% select(-Date_key)

  # -------------------------
  # Write outputs
  # -------------------------
  out_train_file <- file.path(
    out_dir,
    paste0("all_variables_(+)form_parameter_", gamma_id, "_(-)betting_quotes_train_data.csv")
  )
  out_test_file <- file.path(
    out_dir,
    paste0("all_variables_(+)form_parameter_", gamma_id, "_(-)betting_quotes_test_data.csv")
  )

  #readr::write_csv(merged_train, out_train_file)
  #readr::write_csv(merged_test,  out_test_file)

  # -------------------------
  # Basic success diagnostics
  # -------------------------
  # Report how many rows did not find a matching form value.
  n_missing_train <- sum(is.na(merged_train$Home_Form) | is.na(merged_train$Away_Form))
  n_missing_test  <- sum(is.na(merged_test$Home_Form)  | is.na(merged_test$Away_Form))

  message("  -> Wrote: ", basename(out_train_file),
          " | missing form rows: ", n_missing_train)
  message("  -> Wrote: ", basename(out_test_file),
          "  | missing form rows: ", n_missing_test)
}

message("Done. Merged files saved to: ", out_dir)