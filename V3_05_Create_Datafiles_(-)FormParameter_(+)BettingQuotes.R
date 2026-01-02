#What this script is for: 
#1) Read-in and clean data from football-data.co.uk
#2) Combine the data from football-data.co.uk with the data from FBref
#3) Create lags for the last 5 games to create the desired data format (as
#described in the thesis, the values of the variables from the last 5 games
#are used for the models.)
#4) Create data files ready for the models NOT leveraging a form parameter BUT
# leveraging betting odds.

#This script is a copy of "V3_04_Create_Datafiles_(-)FormParameter_(-)BettingQuotes.R".
#It is only altered minimally, such that the columns from the football-data.co.uk,
#which hold the betting odds, are used too. This is not the most elegant way, but the easiest
#and fastest.

#I originally planned on using data from the Premier League from the seasons 
#2000/2001 - 2024/2025, therefore here I import data from these seasons from 
#csv files I downloaded from football-data.co.uk. Later I decided to only use 
#data from 2017/18 and later, because my second source has much more data for 
#this time period.
#
#Store data files in the same directory as the R file and set the working
#directory to source file location, so you do not need an absolute path.

#Use current working directory (.) so no absolute path is needed
dir_path <- "."

#Names of the data files have the following structure: 2000_2001.csv, 2001_2002.csv
#etc. Therefore, file name pattern:
# ^: Start of file name
# $: End of file name
# [0-9]{4}: Exactly 4 numbers
# \\.csv: Exactly ".csv", character for character
files <- list.files(
  path = dir_path,
  pattern = "^[0-9]{4}_[0-9]{4}\\.csv$",
  full.names = TRUE
)

#Create name vector for the files. basename() gets rid of the path, 
#sub() gets rid of the ".csv"
names_vec <- sub("\\.csv$", "", basename(files))
#Store all files as a list of dataframes, accessible via i.e. "dfs["2000_2001"]"
library(readr)
dfs <- setNames(lapply(files, read_csv), names_vec)


#-----Get rid off unnecessary columns-------------------------------------------
#I don't need all columns, only the following:
wanted_cols <- c("HomeTeam", "AwayTeam", "FTR", "Date", "B365H", "B365D", "B365A")

#Create copy of the orignal dfs with name "dfs_reduced", which only holds the wanted columns.
library(dplyr)
dfs_reduced <- lapply(dfs, \(df) select(df, any_of(wanted_cols)))
#Keep the same names for the dataframes in the dfs_reduced list as in the dfs list.
names(dfs_reduced) <- names(dfs)

#Check if any of the wanted columns was missing in the dfs.
missing_report <- lapply(dfs, function(df) setdiff(wanted_cols, names(df)))

missing_overview <- data.frame(
  season       = names(missing_report),
  missing_n    = lengths(missing_report),
  missing_cols = vapply(missing_report, \(x) paste(x, collapse = ", "), "")
)

#Only show seasons with missing columns.
missing_overview[missing_overview$missing_n > 0, ]


#-----Sorting-------------------------------------------------------------------
#Check if all dfs in dfs_reduced are sorted by Date, without modifying the dfs.
#Accept several common formats (should be dd.mm.yyyy but let's be safe).
#Return a Date vector. If the input is already of class "Date",
#it is returned as-is.
parse_date_try <- function(x) {
  # If already a Date object, we can skip parsing entirely.
  if (inherits(x, "Date")) return(x)
  
  #Convert other data types to character first; trim stray whitespace.
  x_chr <- trimws(as.character(x))
  
  #as.Date with tryFormats attempts the listed formats in order and returns NA
  #for any value that matches none of them. After Date checking, I check for NAs.
  as.Date(
    x_chr,
    tryFormats = c(
      "%d.%m.%Y",  #19.08.2002
      "%d/%m/%Y",  #19/08/2002
      "%d/%m/%y",  #19/08/02
      "%Y-%m-%d",  #2002-08-19
      "%d.%m.%y"   #19.08.02
    )
  )
}

#Check if a Date vector is sorted in non-decreasing order (duplicates are allowed
#(A <= B <= C ...).
#NA values are ignored for the ordering check (they do not cause failure).
is_nondecreasing <- function(d) {
  #Work only with non-NA indices; NA dates are skipped in the check.
  idx <- which(!is.na(d))
  
  #Zero or one valid date is trivially "sorted".
  if (length(idx) <= 1) return(TRUE)
  
  #Ensure each subsequent date is >= the previous date.
  all(d[idx][-1] >= d[idx][-length(idx)])
}

#Summarize sort status for each season df in a list.
#Input: a named list of data frames (dfs_reduced), each expected to have a "Date"
#column in some format (they all have - checked before if any columns are missing
#anywhere).
#Output: a data frame where each row summarizes one season:
#   - season:    name of the list element (e.g., "2000_2001")
#   - has_Date:  whether the df contains a "Date" column or not
#   - parsed_na: how many rows failed to parse into a Date
#   - n_rows:    number of rows in the df
#   - is_sorted: TRUE if dates are in non-decreasing order (ignoring NAs)
#   - first:     minimum parsed date (NA if none parsed)
#   - last:      maximum parsed date (NA if none parsed)
check_sorted_by_date <- function(dfs_list) {
  #Map over the list while keeping list names (season labels)
  out <- Map(function(df, season) {
    #If "Date" is missing, I cannot check sorting; fill informative fields.
    if (!"Date" %in% names(df)) {
      return(data.frame(
        season    = season,
        has_Date  = FALSE,
        parsed_na = NA_integer_,
        n_rows    = nrow(df),
        is_sorted = NA,   # not applicable
        first     = NA,   # not applicable
        last      = NA,   # not applicable
        stringsAsFactors = FALSE
      ))
    }
    
    #Parse without changing the df
    d <- parse_date_try(df$Date)
    
    #Build a one-row summary for this season
    data.frame(
      season    = season,
      has_Date  = TRUE,
      parsed_na = sum(is.na(d)),
      n_rows    = nrow(df),
      is_sorted = is_nondecreasing(d),
      first     = if (all(is.na(d))) NA else min(d, na.rm = TRUE),
      last      = if (all(is.na(d))) NA else max(d, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }, dfs_list, names(dfs_list))
  
  #Bind all season summaries into one data.frame
  do.call(rbind, out)
}

# Run the checker (not modifying data here)
date_check <- check_sorted_by_date(dfs_reduced)
date_check

# Focus on seasons that need attention:
# - not sorted by date
# - or had parsing problems
# - or are missing the Date column
date_check[!date_check$is_sorted | date_check$parsed_na > 0 | !date_check$has_Date, ]


#-----Check for NAs-------------------------------------------------------------
#Check all columns in all dfs in dfs_reduced for NAs.
library(tidyr)
library(purrr)

#Map over all dfs in dfs_reduced
na_report <- imap_dfr(
  dfs_reduced,
  function(df, season) {              #Create a tibble for every season
    tibble(
      season = season,
      col    = names(df),
      n_na   = colSums(is.na(df)),    #Sum of NAs per column
      pct_na = colMeans(is.na(df)),   #Percentage of NAs per column
      n_rows = nrow(df)               #Number of rows in the df
    )
  }
)

#Show only columns with NAs, sorted descending by percentage.
problems <- na_report %>% filter(n_na > 0) %>% arrange(desc(pct_na), season, col)
problems


# After working with FBref data I realised that I could only get the best data
# from there for the seasons 2017/18 - 2024/25. Additionally, practically all
# information from the Football UK data is also in the FBref data. Therefore I only need the 
# seasons 2017/18 - 2024/25 and the columns "Date", "HomeTeam", "AwayTeam, "FTR", "B365H", "B365D", "B365A"
# from the footballUK data.

#Define target seasons (2017/18 - 2024/25)
seasons_keep <- sprintf("%d_%d", 2017:2024, 2018:2025)

#Sanity Check: Do all seasons exist as wanted?
missing <- setdiff(seasons_keep, names(dfs_reduced))
if (length(missing)) warning("Did not find in dfs_reduced: ", paste(missing, collapse = ", "))

#Select wanted seasons and combine to a new dataframe
FootballUK_data <- dfs_reduced[intersect(seasons_keep, names(dfs_reduced))] |>
  dplyr::bind_rows()

#Quick check
#str(FootballUK_data)
#View(FootballUK_data)

# There were 2 games with damaged data at FBref. Deleting these two games also
# from the FootballUK data.

FootballUK_data <- FootballUK_data %>%
  filter(
    !(HomeTeam == "Man United" & AwayTeam == "Wolves"  & Date == "13/05/2023"),
    !(HomeTeam == "Chelsea"    & AwayTeam == "Arsenal" & Date == "10/11/2024")
  )

# Find all unique Teams so I can fusion later.
Teams_FootballUK <- data.frame(
  HomeTeam = sort(unique(trimws(as.character(FootballUK_data$HomeTeam)))),
  stringsAsFactors = FALSE
)
View(Teams_FootballUK)

#readr::write_csv2(Teams_FootballUK, "Teams_FootballUK.csv")

#At this point I checked and compared team names from the two sources manually
#and decided to change the names of some teams here so they match with FBref data.

# Mapping: old -> new
map <- c(
  "Brighton"       = "Brighton & Hove Albion",
  "Cardiff"        = "Cardiff City",
  "Huddersfield"   = "Huddersfield Town",
  "Ipswich"        = "Ipswich Town",
  "Leeds"          = "Leeds United",
  "Leicester"      = "Leicester City",
  "Luton"          = "Luton Town",
  "Man City"       = "Manchester City",
  "Man United"     = "Manchester United",
  "Newcastle"      = "Newcastle United",
  "Norwich"        = "Norwich City",
  "Nott'm Forest"  = "Nottingham Forest",
  "Stoke"          = "Stoke City",
  "Swansea"        = "Swansea City",
  "Tottenham"      = "Tottenham Hotspur",
  "West Brom"      = "West Bromwich Albion",
  "West Ham"       = "West Ham United",
  "Wolves"         = "Wolverhampton Wanderers"
)

#Renew names in both Home and Away column
FootballUK_data <- FootballUK_data %>%
  mutate(across(c(HomeTeam, AwayTeam), ~ {
    x <- trimws(as.character(.x))
    y <- map[x]                #Mapped name or NA if no mapping exists
    ifelse(!is.na(y), y, x)    #If no mapping exists: keep original
  }))

any(FootballUK_data$HomeTeam == "Brighton" | FootballUK_data$AwayTeam == "Brighton")
any(FootballUK_data$HomeTeam == "Cardiff" | FootballUK_data$AwayTeam == "Cardiff")
any(FootballUK_data$HomeTeam == "Huddersfield" | FootballUK_data$AwayTeam == "Huddersfield")
any(FootballUK_data$HomeTeam == "Ipswich" | FootballUK_data$AwayTeam == "Ipswich")
any(FootballUK_data$HomeTeam == "Leeds" | FootballUK_data$AwayTeam == "Leeds")
any(FootballUK_data$HomeTeam == "Leicester" | FootballUK_data$AwayTeam == "Leicester")
any(FootballUK_data$HomeTeam == "Luton" | FootballUK_data$AwayTeam == "Luton")
any(FootballUK_data$HomeTeam == "Man City" | FootballUK_data$AwayTeam == "Man City")
any(FootballUK_data$HomeTeam == "Man United" | FootballUK_data$AwayTeam == "Man United")
any(FootballUK_data$HomeTeam == "Newcastle" | FootballUK_data$AwayTeam == "Newcastle")
any(FootballUK_data$HomeTeam == "Norwich" | FootballUK_data$AwayTeam == "Norwich")
any(FootballUK_data$HomeTeam == "Nott'm Forest" | FootballUK_data$AwayTeam == "Nott'm Forest")
any(FootballUK_data$HomeTeam == "Stoke" | FootballUK_data$AwayTeam == "Stoke")
any(FootballUK_data$HomeTeam == "Swansea" | FootballUK_data$AwayTeam == "Swansea")
any(FootballUK_data$HomeTeam == "Tottenham" | FootballUK_data$AwayTeam == "Tottenham")
any(FootballUK_data$HomeTeam == "West Brom" | FootballUK_data$AwayTeam == "West Brom")
any(FootballUK_data$HomeTeam == "West Ham" | FootballUK_data$AwayTeam == "West Ham")
any(FootballUK_data$HomeTeam == "Wolves" | FootballUK_data$AwayTeam == "Wolves")

#readr::write_csv(FootballUK_data, "FootballUK_data_(+)BettingQuotes.csv")
#readr::write_csv2(FootballUK_data, "FootballUK_data_(+)BettingQuotes_DE.csv")

#----------------------------PART TWO: JOIN DATA WITH FBREF DATA----------------------------

#This part ensures the data from the two sources (Football UK and FBref), has
#the same structure in the Date column and joins the two datasources afterwards.
#The data is imported again in the beginning of this part, because the script 
#was not written in one go and creating the csv files above was a way of 
#securing progress.

#library(dplyr)
#Read in data.
footballuk = read.csv("FootballUK_data_(+)BettingQuotes.csv")
#View(footballuk)
fbref = read.csv("FBref_data.csv")
#View(fbref)
#Check type of Date columns.
class(footballuk$Date)
class(fbref$Match_Date)
#Both are characters.
#Change to Date format.
fbref$Match_Date <- as.Date(trimws(fbref$Match_Date))
footballuk$Date <- as.Date(trimws(footballuk$Date), format = "%d/%m/%Y")
anyNA(footballuk)
anyNA(fbref)

#Join the data here.
data_joined = footballuk %>%
  left_join(fbref, by = c("HomeTeam" = "Home_Team",
                          "AwayTeam" = "Away_Team",
                          "Date"     = "Match_Date"))
#View(data_joined)
anyNA(data_joined)

#I do not need the columns match_id and MatchURL. Delete them here.
data_joined = data_joined %>% select(-match_id, -MatchURL)

#Order with oldest games first and youngest last
data_joined = data_joined[order(data_joined$Date), ]

#Slice them into 8 seasons again, because the first 5 games of each season cannot
#be predicted, if I want to take the last 5 games played for prediction. Therefore,
#I need the data season-wise.

s2017_18 = data_joined[1:380,]
s2018_19 = data_joined[381:760,]
s2019_20 = data_joined[761:1140,]
s2020_21 = data_joined[1141:1520,]
s2021_22 = data_joined[1521:1900,]
s2022_23 = data_joined[1901:2280,]
s2023_24 = data_joined[2281:2659,]#one game deleted from this season
s2024_25 = data_joined[2660:3038,]#one game deleted from this season too

#-------------------PART THREE: CREATE DATAFILES FOR LAGGED VARIABLES--------------------------------

#This part structures the data in a way, such that every row represents one
#game, but the variables in this row represent the last 5 games. E.g. for a game
#from matchweek 10, the variables in its row represent the games from matchweeks
#9, 8, 7, 6 and 5 (except for the result, this will be from matchweek 10). I do 
#this, because I want to predict the games based on the gamestats from the last
#5 games.
#library(dplyr)
library(stringr)

#This function will do the work.
make_lagged_df <- function(df) {
  #Create a dataframe with only the home variables.
  home_tf <- df %>%
    transmute(
      Team     = HomeTeam,
      Opponent = AwayTeam,
      Date,
      side     = "H",
      across(starts_with("Home_"))
    ) %>%
    rename_with(~ str_remove(.x, "^Home_"), starts_with("Home_"))
  
  #Create a dataframe with only the away variables.
  away_tf <- df %>%
    transmute(
      Team     = AwayTeam,
      Opponent = HomeTeam,
      Date,
      side     = "A",
      across(starts_with("Away_"))
    ) %>%
    rename_with(~ str_remove(.x, "^Away_"), starts_with("Away_"))
  
  #Bind the home and the away dataframe row-wise (put them under each other, now
  #there are two rows per game again.)
  team_games <- bind_rows(home_tf, away_tf)
  
  #stat_cols only receives the columns with gamestats, which will be lagged later.
  #The 4 columns Team, Opponent, Date and side are excluded from stat_cols.
  stat_cols <- setdiff(names(team_games), c("Team","Opponent","Date","side"))
  
  #Now the gamestats are lagged.
  lagged <- team_games %>%
    #Sort the games chronologically by date within the teams, so they can be
    #lagged the right way.
    arrange(Team, Date) %>%
    #Group by teams, such that the teams do not influence each other and the
    #right games are picked for lagging.
    group_by(Team) %>%
    #For each column in stats_col 5 new columns (the lagged ones) are created.
    mutate(
      across(
        all_of(stat_cols),
        list(`1` = ~lag(.x, 1),
             `2` = ~lag(.x, 2),
             `3` = ~lag(.x, 3),
             `4` = ~lag(.x, 4),
             `5` = ~lag(.x, 5)),
        #name them by keeping the column name and adding the suffixes _1, _2, _3, _4, _5  
        .names = "{.col}_{.fn}"
      ),
      #Mark rows, in which at least 5 earlier games exist (from matchweek 6 on).
      has5 = row_number() > 5
    ) %>%
    ungroup()
  
  #Cut out the lagged package from a home perspective.
  home_lags <- lagged %>%
    filter(side == "H") %>%
    select(Team, Opponent, Date, has5, matches("_(1|2|3|4|5)$")) %>%
    rename(Home_has5 = has5) %>%
    rename_with(~ paste0("Home_", .x), matches("_(1|2|3|4|5)$"))
  
  #Cut out the lagged package from a away perspective.
  away_lags <- lagged %>%
    filter(side == "A") %>%
    select(Team, Opponent, Date, has5, matches("_(1|2|3|4|5)$")) %>%
    rename(Away_has5 = has5) %>%
    rename_with(~ paste0("Away_", .x), matches("_(1|2|3|4|5)$"))
  
  #Now join the lagged gamestats to create one row per game again. Start with 
  #the original df (only keeping the key-columns, FTR, B356H, B365A and B365D) and join the lagged
  #home and away stats.
  df %>%
    select(HomeTeam, AwayTeam, Date, FTR, B365H, B365D, B365A) %>%
    left_join(home_lags, by = c("HomeTeam"="Team","AwayTeam"="Opponent","Date"="Date")) %>%
    left_join(away_lags, by = c("AwayTeam"="Team","HomeTeam"="Opponent","Date"="Date")) %>%
    filter(Home_has5, Away_has5) %>%
    select(-Home_has5, -Away_has5)
}

seasons <- c("s2017_18","s2018_19","s2019_20","s2020_21",
             "s2021_22","s2022_23","s2023_24","s2024_25")

#Iterate all seasons over the function "make_lagged_df" to create the lagged dfs
#for all seasons and add the suffix "_new" to prohibit confusion.
for (nm in seasons) {
  if (exists(nm, inherits = TRUE)) {
    assign(paste0(nm, "_new"), make_lagged_df(get(nm)))
  } else {
    warning(sprintf("Object %s not found; skipping.", nm))
  }
}

#Bind the first 7 seasons together, because they are the train data. The 8th
#season is test data.
train_data <- bind_rows(
  s2017_18_new,
  s2018_19_new,
  s2019_20_new,
  s2020_21_new,
  s2021_22_new,
  s2022_23_new,
  s2023_24_new
)

#Create csv files to be imported later. (One German version for train and test
#data each is safed too, because it makes it easier to check the result manually.)
#write.csv(train_data, "data_(-)Form Parameter_(+)Betting Quotes/all_variables_(-)form_parameter_(+)betting_quotes_train_data.csv", row.names = FALSE)
#write.csv2(train_data, "data_(-)Form Parameter_(+)Betting Quotes/all_variables_(-)form_parameter_(+)betting_quotes_train_data_DE.csv", row.names = FALSE)
#write.csv(s2024_25_new, "data_(-)Form Parameter_(+)Betting Quotes/all_variables_(-)form_parameter_(+)betting_quotes_test_data.csv", row.names = FALSE)
#write.csv2(s2024_25_new, "data_(-)Form Parameter_(+)Betting Quotes/all_variables_(-)form_parameter_(+)betting_quotes_test_data_DE.csv", row.names = FALSE)