#What this script is for:
# 1) Reads:  FootballUK_data_(-)BettingQuotes.csv  (same folder as this script)
# 2) Computes team form parameter for multiple gamma values: (as described in the thesis)
#      - 0.05, 0.10, ..., 1.00
#      - plus 0.33
# 3) Resets all team forms to 1 at the start of EACH season.
# 4) Writes, for each gamma:
#      - training file (seasons 2017/18–2023/24)
#      - test file     (season  2024/25)
#    Each output = original columns + Home_Form, Away_Form (PRE-match values)
# 5) ROUNDS Home_Form and Away_Form to 5 decimal places before saving.

# Define input (file needs to be stored in the same folder as the script)
input_file <- "FootballUK_data_(-)BettingQuotes.csv"
# Define output directory
output_dir <- "data_onlyFormParameter"

# Round form features to this many decimal places
round_digits <- 5

# Desired values for the hyperparameter gamma
gamma_values <- c(seq(0.05, 1.00, by = 0.05), 0.33)

# Row ranges per season (ROW NUMBERS IN THE DATAFRAME, header excluded by read.csv)
# i.e., after reading the CSV, row 1 == first match row
seasons <- list(
  s2017 = 1:380,       # 2017/2018
  s2018 = 381:760,     # 2018/2019
  s2019 = 761:1140,    # 2019/2020
  s2020 = 1141:1520,   # 2020/2021
  s2021 = 1521:1900,   # 2021/2022
  s2022 = 1901:2280,   # 2022/2023
  s2023 = 2281:2659,   # 2023/2024 (one match excluded due to missing data)
  s2024 = 2660:3038    # 2024/2025 (one match excluded due to missing data)
)

# Define train and test seasons
train_seasons <- c("s2017","s2018","s2019","s2020","s2021","s2022","s2023")
test_seasons  <- c("s2024")

# Create output folder (if it does not exist already)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Load data
data <- read.csv(input_file, stringsAsFactors = FALSE)

# Set the required columns
required_cols <- c("HomeTeam", "AwayTeam", "FTR", "Date")
if (!all(required_cols %in% names(data))) {
  stop("Missing required columns: ",
       paste(setdiff(required_cols, names(data)), collapse = ", "))
}

# Sanity check: is dataset long enough for the declared row ranges?
max_needed <- max(unlist(seasons))
if (nrow(data) < max_needed) {
  stop("Dataset has only ", nrow(data), " rows, but season ranges require at least ", max_needed, " rows.")
}

# Create a small helper to get the value of gamma in the right format for the 
# file names. E.g. for gamma = 0.05 the created data file shall have a "005"
# in the file name.
format_gamma <- function(g) sprintf("%03d", round(g * 100))

# FORM UPDATE RULE
update_form <- function(f_h, f_a, result, gamma) {
  if (is.na(result)) return(c(h = f_h, a = f_a))

  r <- toupper(trimws(as.character(result)))

  if (r == "H") {         # Home win
    c(h = f_h + gamma * f_a,
      a = (1 - gamma) * f_a)

  } else if (r == "A") {  # Away win
    c(h = (1 - gamma) * f_h,
      a = f_a + gamma * f_h)

  } else if (r == "D") {  # Draw
    c(h = f_h - gamma * (f_h - f_a),
      a = f_a - gamma * (f_a - f_h))

  } else {
    # Unexpected value -> no update
    c(h = f_h, a = f_a)
  }
}

# Process one season here.
# This function:
# - iterates in the given row order
# - stores pre-match form into Home_Form / Away_Form
# - updates internal form state after each match
add_form_columns_one_season <- function(df, gamma) {

  n <- nrow(df)
  if (n == 0) {
    df$Home_Form <- numeric(0)
    df$Away_Form <- numeric(0)
    return(df)
  }

  # Output vectors for PRE-match values
  Home_Form <- rep(NA_real_, n)
  Away_Form <- rep(NA_real_, n)

  # Use an environment with key = team name and value = current form
  forms <- new.env(parent = emptyenv())

  get_form <- function(team) {
    if (!exists(team, envir = forms, inherits = FALSE)) {
      assign(team, 1, envir = forms)  # lazy init to 1 at first appearance
    }
    get(team, envir = forms, inherits = FALSE) # get the current form
  }

  set_form <- function(team, value) { # safe the updated form
    assign(team, value, envir = forms)
  }
  
  #Main: Iterate over all games of a season and update the form after each game.
  for (i in seq_len(n)) {

    # Clean team names and full time result
    h <- trimws(as.character(df$HomeTeam[i]))
    a <- trimws(as.character(df$AwayTeam[i]))
    r <- toupper(trimws(as.character(df$FTR[i])))

    # 1) Save PRE-match form
    if (!is.na(h) && nzchar(h)) Home_Form[i] <- get_form(h)
    if (!is.na(a) && nzchar(a)) Away_Form[i] <- get_form(a)

    # 2) Update AFTER the match (only if both team names exist and result is valid)
    if (!is.na(h) && nzchar(h) && !is.na(a) && nzchar(a) && r %in% c("H","A","D")) {
      fh <- get_form(h) #get pre-match form
      fa <- get_form(a)
      new_vals <- update_form(fh, fa, r, gamma) #call update function here
      set_form(h, new_vals[["h"]]) #update form parameters in the environment
      set_form(a, new_vals[["a"]])
    }
  }

  # Append columns to df (keeping all original columns)
  df$Home_Form <- Home_Form
  df$Away_Form <- Away_Form
  df
}

# Loop over all desired gamma values and create csv files
for (gamma in gamma_values) {

  # Sanity check
  if (gamma <= 0 || gamma > 1) stop("Gamma must be in (0, 1]. Found: ", gamma)

  cat("Processing gamma =", gamma, "\n")

  # Compute season-by-season (reset forms each season!)
  season_results <- list()

  # Iterate over all seasons
  for (season_name in names(seasons)) {

    # Pick the rows of the current season
    rows <- seasons[[season_name]]

    # Cut the rows of the current seasons out of "data"
    season_df <- data[rows, , drop = FALSE]

    # Store the results of the current season
    season_results[[season_name]] <- add_form_columns_one_season(season_df, gamma)
  }

  # Build train/test outputs (in season order)
  train_df <- do.call(rbind, season_results[train_seasons])
  test_df  <- do.call(rbind, season_results[test_seasons])

  # Create output file names
  g_str <- format_gamma(gamma)
  train_file <- file.path(output_dir, paste0("gamma_", g_str, "_train.csv"))
  test_file  <- file.path(output_dir, paste0("gamma_", g_str, "_test.csv"))

  # Safe output files
  write.csv(train_df, train_file, row.names = FALSE)
  write.csv(test_df, test_file, row.names = FALSE)
}

cat("Done: All gamma values processed and files written to: ", output_dir, "\n")