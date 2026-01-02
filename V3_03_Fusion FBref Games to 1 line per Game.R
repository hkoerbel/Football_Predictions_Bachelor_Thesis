#What this script is for:
#The previously collected data from FBref is in an unstructured form. This script
#transforms all the data from FBref in such a way, that in the end a data file,
#which has one column for each of the collected variables for both the home and
#away team of a game respectively, resulting in only one row of data for each game,
#is obtained.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(purrr); library(tidyr)
})

#Set the directory, where the .rds files for the different games are stored.
#Using my scripts it is called "fbref_cache".
CACHE_DIR <- "fbref_cache"
stopifnot(dir.exists(CACHE_DIR))

#Get paths of the .rds files (with their names).
rds_files <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
#Get rid of the index file.
rds_files <- rds_files[!grepl("^fetch_index", basename(rds_files), ignore.case = TRUE)]
stopifnot(length(rds_files) > 0)

#Extract Match IDs from the filepath
match_ids <- tools::file_path_sans_ext(basename(rds_files))

#Load indices to map Match URLs.
matchurl_map <- {
  #Create two candidate file paths (one for .rds, one for .csv)
  cand <- file.path(CACHE_DIR, c("fetch_index.rds","fetch_index.csv"))
  #Filter for existing files (should be two, because both the .rds and the .csv
  #should exist).
  cand <- cand[file.exists(cand)]
  #If none of the files was found, create a tibble with match_ids and set 
  #MatchURLs to NA.
  if (length(cand) == 0) {
    tibble(match_id = match_ids, MatchURL = NA_character_)
  } 
  #Else check if first file in cand is the .rds file. If true, load it into idx.
  else if (grepl("\\.rds$", cand[1], ignore.case = TRUE)) {
    idx <- readRDS(cand[1])
    #Check if a column "match_id" does NOT exist in the file, now loaded in idx.
    if (!"match_id" %in% names(idx)) {
      #Enter if column "match_id" does NOT exist. Now check, if column "cache_file"
      #exists. If it does, create the column match_id in idx by extracting the IDs
      #from the filename.
      if ("cache_file" %in% names(idx)) {
        idx <- idx %>% mutate(match_id = tools::file_path_sans_ext(basename(cache_file)))
      } 
      #If "cache_file" does also not exist, check for column "MatchURL" and extract
      #the Match ID from there.
      else if ("MatchURL" %in% names(idx)) {
        idx <- idx %>% mutate(match_id = str_match(MatchURL, "(?<=/matches/)([A-Za-z0-9]+)")[,2])
      }
    }
    #Store the intersection of the wanted columns from in idx in keep.
    keep <- intersect(c("match_id","MatchURL"), names(idx))
    #If wanted columns not found, create a tibble with match_ids and set MatchURL
    #to NA.
    if (length(keep) == 0) tibble(match_id = match_ids, MatchURL = NA_character_)
    #Else, get rid of duplicated match_ids. Only keep the row, where it is 
    #mentioned first.
    else idx[, keep, drop = FALSE] %>% distinct(match_id, .keep_all = TRUE)
  } 
  #Second path: No .rds file. Try csv file.
  else {
    #Try semicolon first, comma second. If both fails set idx to NULL.
    idx <- tryCatch(readr::read_csv2(cand[1], show_col_types = FALSE),
                    error = function(e) tryCatch(readr::read_csv(cand[1], show_col_types = FALSE),
                                                 error = function(e2) NULL))
    #If idx is NULL create a tibble with match_ids and set MatchURL to NULL.
    #After that go through the same checks as described above with the .rds file
    #to find/create match_id.
    if (is.null(idx)) tibble(match_id = match_ids, MatchURL = NA_character_) else {
      if (!"match_id" %in% names(idx)) {
        if ("cache_file" %in% names(idx)) {
          idx <- idx %>% mutate(match_id = tools::file_path_sans_ext(basename(cache_file)))
        } else if ("MatchURL" %in% names(idx)) {
          idx <- idx %>% mutate(match_id = str_match(MatchURL, "(?<=/matches/)([A-Za-z0-9]+)")[,2])
        }
      }
      keep <- intersect(c("match_id","MatchURL"), names(idx))
      if (length(keep) == 0) tibble(match_id = match_ids, MatchURL = NA_character_)
      else idx[, keep, drop = FALSE] %>% distinct(match_id, .keep_all = TRUE)
    }
  }
}

#Define the columns that I want from the different data collections.
summary_common <- c(
  "Ast","PK","PKatt","Sh","SoT","Touches","Tkl","Int","Blocks",
  "npxG_Expected","xAG_Expected","SCA_SCA","GCA_SCA",
  "Att_Passes","Cmp_percent_Passes","PrgP_Passes",
  "Carries_Carries","PrgC_Carries","Att_Take_Ons"
)
summary_home_req <- c("Match_Date","Home_Team","Home_Score","Home_xG","Home_Yellow_Cards","Home_Red_Cards")
summary_away_req <- c("Away_Team","Away_Score","Away_xG","Away_Yellow_Cards","Away_Red_Cards")

possession_req <- c(
  "Def Pen_Touches","Def 3rd_Touches","Mid 3rd_Touches","Att 3rd_Touches",
  "Att Pen_Touches","Succ_percent_Take_Ons","Tkld_percent_Take_Ons",
  "TotDist_Carries","PrgDist_Carries","Final_Third_Carries",
  "CPA_Carries","Mis_Carries","Dis_Carries"
)

team_req <- c(
  "Home_Possession","Home_Fouls","Home_Corners","Home_Crosses","Home_Tackles",
  "Home_Aerials_Won","Home_Clearances","Home_Offsides","Home_Goal_Kicks",
  "Home_Throw_Ins","Home_Long_Balls",
  "Away_Possession","Away_Fouls","Away_Corners","Away_Crosses","Away_Tackles",
  "Away_Aerials_Won","Away_Clearances","Away_Offsides","Away_Goal_Kicks",
  "Away_Throw_Ins","Away_Long_Balls"
)

#Set the wanted order.
target_order <- c(
  "match_id","MatchURL",
  # summary Home
  "Match_Date","Home_Team","Home_Score","Home_xG","Home_Yellow_Cards","Home_Red_Cards",
  paste0("Home_", summary_common),
  # summary Away
  "Away_Team","Away_Score","Away_xG","Away_Yellow_Cards","Away_Red_Cards",
  paste0("Away_", summary_common),
  # possession Home
  paste0("Home_", possession_req),
  # possession Away
  paste0("Away_", possession_req),
  # team_stats
  team_req
)

#Helper function to ensure the existence of all columns.
ensure_cols <- function(df, cols) {
  #If the given df is null, create an emtpy tibble.
  if (is.null(df)) df <- tibble()
  #Store the difference between the wanted columns and the columns in the given
  #df in "miss".
  miss <- setdiff(cols, names(df))
  #If a column is missing (length(miss) > 0) add an empty column with the according name.
  if (length(miss)) for (m in miss) df[[m]] <- NA
  #Return df
  df
}

#Helper to extract the columns from summary data collection (first of three from
#FBref), one line per side (Home and Away). 
extract_summary_side <- function(summary_df, side = c("Home","Away")) {
  #Get side.
  side <- match.arg(side)
  #Allocate metrics.
  common_metrics <- summary_common
  base_home <- summary_home_req
  base_away <- summary_away_req
  
  #Assign the needed columns to "need", depending on if its the home or away side.
  need <- if (side == "Home") c("Home_Away", base_home, common_metrics)
  else                 c("Home_Away", base_away, common_metrics)
  
  #Hand df over to ensure_cols to make sure it has all needed columns.
  df <- ensure_cols(summary_df, need)
  #Only keep the home or the away row from df, depending on the current side.
  #If "Home_Away" does not exist (if should) then assign a 0 row dataframe with
  #the same columns.
  df <- if ("Home_Away" %in% names(df)) dplyr::filter(df, Home_Away == side) else df[0, ]
  #If df has 0 rows, add a dummy row with all needed columns and fill it with NAs.
  if (nrow(df) == 0) df <- tibble(!!!setNames(rep(list(NA), length(need)), need))
  
  
  if (side == "Home") {
    #Select all metrics for the home team and rename those from the common metrics
    #(where there is not a "Home_" prefix already there) so they get the "Home_"
    #prefix.
    df %>% select(all_of(base_home), all_of(common_metrics)) %>%
      rename_with(~ paste0("Home_", .), all_of(common_metrics)) %>% slice(1)
  } else {
    #Do the same as in the 2 lines above for the Away team.
    df %>% select(all_of(base_away), all_of(common_metrics)) %>%
      rename_with(~ paste0("Away_", .), all_of(common_metrics)) %>% slice(1)
  }
}

#Helper to extract the columns from possession data collection (second of three 
#from FBref), one line per side (Home and Away). Works the same way as the one
#for the summary data collection above.
extract_possession_side <- function(pos_df, side = c("Home","Away")) {
  side <- match.arg(side)
  need <- c("Home_Away", possession_req)
  
  df <- ensure_cols(pos_df, need)
  df <- if ("Home_Away" %in% names(df)) dplyr::filter(df, Home_Away == side) else df[0, ]
  if (nrow(df) == 0) df <- tibble(!!!setNames(rep(list(NA), length(need)), need))
  
  df %>% select(all_of(possession_req)) %>%
    rename_with(~ paste0(side, "_", .), all_of(possession_req)) %>%
    slice(1)
}

prepare_team_stats <- function(ts) {
  #There was a problem with the way how "Clearences" was written. This ensures
  #it has the same way of writing everywhere.
  if (!"Home_Clearances" %in% names(ts) && "Home_Clearences" %in% names(ts))
    ts$Home_Clearances <- ts$Home_Clearences
  if (!"Away_Clearances" %in% names(ts) && "Away_Clearences" %in% names(ts))
    ts$Away_Clearances <- ts$Away_Clearences
  ensure_cols(ts, team_req) %>% select(all_of(team_req)) %>% slice(1)
}

#Now the gamestats are really extracted by using the functions defined before. 
build_gamestats_from_cache <- function(cache_path, match_id = NA_character_, match_url = NA_character_) {
  #Read in the .rds file.
  tri <- readRDS(cache_path)
  #Split the three parts of the data collections accordingly.
  sm <- tri$summary; po <- tri$possession; ts <- tri$team_stats
  
  #Extract here.
  home_sum <- extract_summary_side(sm, "Home")
  away_sum <- extract_summary_side(sm, "Away")
  home_pos <- extract_possession_side(po, "Home")
  away_pos <- extract_possession_side(po, "Away")
  team_pre <- prepare_team_stats(ts)
  
  #Combine the data collections columnwise.
  dplyr::bind_cols(
    tibble(match_id = match_id, MatchURL = match_url),
    home_sum, away_sum, home_pos, away_pos, team_pre
  )
}

#Now we build one line per game.
#Create an URL lookup with match_id as key.
url_lookup <- setNames(matchurl_map$MatchURL, matchurl_map$match_id)

#This will be the final dataframe for FBref data with only 1 line of data per game.
FBref_data <- pmap_dfr(
  #Iterate over three inputs of the same length (rds_files, match_ids, urls) at 
  #the same time with pmap_dfr().
  list(rds_files, match_ids, unname(url_lookup[match_ids])),
  #Worker function per line.
  function(cf, mid, url) {
    tryCatch(
      #Normal case: Build a one liner with gamestats per game from the cachfiles
      #and match_id.
      #If URL is NA assign an NA from type character so it fits the rest.
      build_gamestats_from_cache(cf, mid, ifelse(is.na(url), NA_character_, url)),
      #If something unexpected happens, write to the console in what file and 
      #what the error was.
      error = function(e) {
        warning("Error at ", cf, " -> ", conditionMessage(e))
        # Fallback: Handover a row with only IDs and empty target columns.
        empty_cols <- setdiff(target_order, c("match_id","MatchURL"))
        dplyr::bind_cols(
          tibble(match_id = mid, MatchURL = url),
          as_tibble(setNames(replicate(length(empty_cols), NA, simplify = FALSE), empty_cols))
        )
      }
    )
  }
)

#Order FBref_data in the desired order.
FBref_data <- FBref_data %>%
  relocate(any_of(target_order)) %>%
  select(any_of(target_order), everything())

#Print the dimensions to the console.
cat("FBref_data Dimensions: ", nrow(FBref_data), " x ", ncol(FBref_data), "\n")

#Take a short look.
print(dplyr::glimpse(FBref_data))
View(FBref_data)
anyNA(FBref_data)

#There were 2 games for with the transformation did not work. Getting rid of 
#them here.
bad_ids <- c("f5d61382", "2874d56e")
FBref_data <- FBref_data[ !FBref_data$match_id %in% bad_ids, ]
anyNA(FBref_data)

#safe the file
#readr::write_csv(FBref_data, "FBref_data.csv")
#readr::write_csv2(FBref_data, "FBref_data_DE.csv")

#Find all unique teams here, so I can fusion this dataset later with the one
#from Football UK.
Teams_FBref <- data.frame(
  Home_Team = sort(unique(trimws(as.character(FBref_data$Home_Team)))),
  stringsAsFactors = FALSE
)
View(Teams_FBref)

#readr::write_csv2(Teams_FBref, "Teams_FBref.csv")
