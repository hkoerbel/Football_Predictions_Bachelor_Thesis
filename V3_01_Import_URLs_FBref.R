#What this script is for:
#Data from FBref can be accessed via specific sub-pages of their website. For 
#each game one URL is needed in order to collect data for this game via this 
#URL later. Therefore, this script collects game URLs for games from FBref so I 
#can collect data in a next step (next script).

if (!requireNamespace("worldfootballR", quietly = TRUE)) install.packages("worldfootballR")
suppressPackageStartupMessages({
  library(worldfootballR)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(httr)
})

#Initialise a friendly User-Agent
httr::set_config(httr::user_agent("FBref URL collector via worldfootballR"))

#Define a function for retries with longer pauses, in case the Server does not
#permit access (happened when I tried to access it to often in a short time period)
with_backoff <- function(expr_fun, pauses = c(4, 10, 25, 60)) {
  for (i in seq_along(pauses)) {
    wait <- pauses[i]
    #tryCatch - do not break in case of error or warning but return NULL
    out <- tryCatch(expr_fun(wait), error = function(e) NULL, warning = function(w) NULL)
    #Success: character longer than 0 (expecting URLs)
    if (is.character(out) && length(out) >= 1) return(out)
    Sys.sleep(wait)                                              
  }
  #Assign NULL in case all tries fail
  NULL
}

#Collect URLs for the games
get_season_urls <- function(season_end_year) {
  message("Collecting URLs via fb_match_urls() for season_end_year = ", season_end_year, " ...")
  urls <- with_backoff(function(pz) {
    #Function from worldfootballR
    fb_match_urls(country = "ENG", gender = "M",
                  season_end_year = season_end_year, tier = "1st",
                  time_pause = pz)
  })
  #Return right datatypes in case of problems, so the binding works later
  if (is.null(urls) || length(urls) == 0) {
    warning("No URLs for season_end_year = ", season_end_year)
    return(tibble(season_end_year = integer(), MatchURL = character()))
  }
  #Return season and URLs in case of success
  tibble(season_end_year = season_end_year, MatchURL = unique(urls))
}

# -----Main
SEASONS <- 2018:2025  # all seasons from 2017/18 - 2024/25 (end year matters)
ALL_LIST <- vector("list", length(SEASONS))

for (i in seq_along(SEASONS)) {
  yr <- SEASONS[i]
  df_yr <- get_season_urls(yr)
  #Info how many URLs were found for a specific season
  message("  -> ", nrow(df_yr), " URLs for ", yr)
  #Store season tibbles in list
  ALL_LIST[[i]] <- df_yr
  
  #Cooldown between seasons
  Sys.sleep(3)
}

#Bind URLs of all seasons together
urls_raw <- bind_rows(ALL_LIST)

#Add new columns "match_id"
urls_out <- urls_raw %>%
  mutate(match_id = stringr::str_match(MatchURL, "(?<=/matches/)([^/]+)")[, 2]) %>%
  distinct(MatchURL, .keep_all = TRUE) %>% #ensure no duplicates
  relocate(season_end_year, match_id, MatchURL) #change order of columns

# -----Store the URLs as a file
csv_path <- "FBref_match_urls_PremierLeague_2017_18_to_2024_25.csv"

#readr::write_csv2(urls_out, csv_path)

message("Done Number of URLs: ", nrow(urls_out))