#What this script is for:
#In the previous script, URLs to FBref sub-pages were collected. This script
#leverages these URLs and collects the wanted data from FBref.

#This script collects data from FBref using the game URLs, which were collected before.

if (!requireNamespace("worldfootballR", quietly = TRUE)) install.packages("worldfootballR")
suppressPackageStartupMessages({
  library(worldfootballR)
  library(dplyr)
  library(readr)
  library(stringr)
})

#File to read in
IN_CSV   <- "FBref_match_urls_PremierLeague_2017_18_to_2024_25.csv"
#Folder where data will be stored
OUT_DIR  <- "fbref_cache"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

#Read in list of URLs
urls_df <- readr::read_csv2(IN_CSV, show_col_types = FALSE)

#Make sure match_id column exists (should not be necessary, but better to be safe than sorry)
if (!"match_id" %in% names(urls_df)) {
  urls_df <- urls_df %>%
    mutate(match_id = stringr::str_match(MatchURL, "(?<=/matches/)([^/]+)")[, 2])
}

#Clean up (should also not be necessary, but again better to be safe)
urls_df <- urls_df %>%
  mutate(
    MatchURL = stringr::str_trim(MatchURL), #trim white spaces
    match_id = ifelse(is.na(match_id) | match_id == "",
                      stringr::str_match(MatchURL, "(?<=/matches/)([A-Za-z0-9]+)")[, 2],
                      match_id)
  ) %>%
  filter(!is.na(MatchURL), !is.na(match_id)) %>% #get rid of NAs (there should not be any)
  distinct(match_id, .keep_all = TRUE) #ensure no duplicates

cat("Number games:", nrow(urls_df), "\n")

#Define waiting time of 7 seconds in between requests to FBref, because with too
#many requests in too little time I was blocked from accessing the page (it
#seems only 10 requests per minute are allowed).
#Takes about 36h to collect the data like this but it works.
WAIT <- 7

#Write a specific error message if the problem of too many requests (429) happens 
#and the original error message if other errors occur.
call_or_msg <- function(expr) {
  tryCatch(expr, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("429", msg)) {
      stop("HTTP 429 (Too Many Requests). Wait 30 minutes and try again.", call. = FALSE)
    } else {
      stop(msg, call. = FALSE)
    }
  })
}

#Load one game, consisting of 3 collections, with function from worldfootballR
fetch_match_triplet <- function(match_url) {
  summary_df <- call_or_msg(
    fb_advanced_match_stats(match_url = match_url, stat_type = "summary",
                            team_or_player = "team", time_pause = WAIT)
  )
  Sys.sleep(WAIT)
  
  possession_df <- call_or_msg(
    fb_advanced_match_stats(match_url = match_url, stat_type = "possession",
                            team_or_player = "team", time_pause = WAIT)
  )
  Sys.sleep(WAIT)
  
  team_stats_df <- call_or_msg(
    fb_team_match_stats(match_url = match_url, time_pause = WAIT)
  )
  Sys.sleep(WAIT)
  
  list(summary = summary_df, possession = possession_df, team_stats = team_stats_df)
}

#Create index, fill later
index <- tibble(
  match_id = character(),
  MatchURL = character(),
  nrows_summary = integer(),
  nrows_possession = integer(),
  nrows_team_stats = integer(),
  cache_file = character()
)

#Run through urls_df
for (i in seq_len(nrow(urls_df))) {
  id  <- urls_df$match_id[i]
  url <- urls_df$MatchURL[i]
  cache_path <- file.path(OUT_DIR, paste0(id, ".rds"))
  
  #Print progress
  cat(sprintf("[%d/%d] %s\n", i, nrow(urls_df), url))
  
  #Check if file already exists, otherwise get it from the website
  if (file.exists(cache_path)) {
    triplet <- readRDS(cache_path)
  } else {
    triplet <- fetch_match_triplet(url)
    saveRDS(triplet, cache_path)
  }
  
  #Create index with information about the 3 data collections of every game
  index <- add_row(
    index,
    match_id = id,
    MatchURL = url,
    nrows_summary    = if (is.data.frame(triplet$summary))    nrow(triplet$summary)    else NA_integer_,
    nrows_possession = if (is.data.frame(triplet$possession)) nrow(triplet$possession) else NA_integer_,
    nrows_team_stats = if (is.data.frame(triplet$team_stats)) nrow(triplet$team_stats) else NA_integer_,
    cache_file = cache_path
  )
}

#Store information about the 3 data collections per game
#readr::write_csv(index, file.path(OUT_DIR, "fetch_index.csv"))
saveRDS(index, file.path(OUT_DIR, "fetch_index.rds"))