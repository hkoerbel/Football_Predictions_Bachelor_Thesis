# Football_Predictions_Bachelor_Thesis

# Project Overview
The primary goal of this project is to predict football match outcomes (Home Win, Draw, Away Win) for the 2024/25 season of the English Premier League. The project compares different model architectures and evaluates them using both statistical metrics and economic betting strategies. For detailed information the thesis is attached in the repository.

# Key Features & Methodology
- Machine Learning Models: Implementation of XGBoost and Random Forest (via R packages randomForest, ranger, and xgboost).
- Feature Engineering:
    - Utilization of 94 play-specific variables per team, covering offensive, defensive, and playmaking statistics.
    - Integration of the last 5 games as temporal components to allow the models to learn historical importance.
    - Calculation of a dynamic form parameter based on a "stealing" parameter γ, which reflects recent team strength relative to opponents.
    - Inclusion of betting odds as explanatory variables.
- Data Sources: Data was retrieved from football-data.co.uk and fbref.com (scraped via the worldfootballR package).

# Core Results
- Performance Enhancement: The results indicate that including both betting odds and the form parameter improves prediction quality, as measured by the Ranked Probability Score (RPS).
- Model Comparison: While XGBoost models were more consistent in predicting at least some draws, Random Forest models achieved overall lower (better) RPS values.
- RPS: The best-performing model achieved a RPS of 0.2144, almost reaching prediction quality as good as the bookmakers (0.2023).
- Accuracy: The best-performing model reached an accuracy of 52.89%, coming very close to the results achieved by the bookmakers (53.50%).
- Economic Evaluation: A positive return of up to 65% was achieved over the course of one season. However, the strategy is not considered risk-safe, as the 95% confidence interval for returns included negative values.

# Code
The codefiles contained serve the following purposes.

## V3_01_Import_URLs_FBref
One of the data sources for this project was FBref. Data can there be accessed via specific sub-pages on the website, which hold information and statistics to different games. Therefore, for each game, an URL is needed before data can be collected. This script collects the URLs for the games of interest.


## V3_02_Import_Data_FBref
This script leverages the URLs collected in *V3_01_Import_URLs_FBref* to import data from FBref.


## V3_03_Fusion_FBref_Games_to_1_line_per_Game
The data collected in *V3_02_Import_Data_FBref* is in an unstructured format. This script transforms all the data collected from FBref, such that in the end a data file with only one row of data per game is obtained.


## V3_04_Create_Datafiles_(-)FormParameter_(-)BettingQuotes
The second data source for this project was football-data.co.uk. The data from this website could be downloaded in csv format, with one file per season.
This script combines this data with the data from FBref. Furthermore, lags for the last five games are created, meaning that after the transformations of this script, each row representing a game does not hold the game statistics of this game, but of the five previous games. With this data files ready for prediction models not leveraging a form parameter nor betting odds are created.


## V3_05_Create_Datafiles_(-)FormParameter_(+)BettingQuotes
This script has the same functionality as *V3_04_Create_Datafiles_(-)FormParameter_(-)BettingQuotes* (combining both data sources and creating lags), with the difference, that in the end data files ready for prediction models not leveraging a form parameter but betting odds are created.


## V3_06_Create_Datafiles_FormParameters
This script calculates the form parameters of the home and away team for all games with different values for the hyperparameter γ as described in the thesis. In the end, data files containing these form parameters are stored.


## V3_07_Create_Datafiles_(+)FormParameter_(-)BettingQuotes
This script creates data files ready for models leveraging a form parameter but no betting odds, building on the files created in the previous scripts.


## V3_08_Create_Datafiles_(+)FormParameter_(+)BettingQuotes
This script creates data files ready for models leveraging a form parameter and betting odds, building on the files created in the previous scripts.


## V3_09_Create_Models
In this script, the prediction models are trained. For each version one XGBoost and two Random Forest models (one time with the randomforest package, one time with the ranger package) are trained. Additionally, performance metrics are calculated and the models safed as rds files.


## V3_10_Calculate_Bookies_Performance
This script leverages the betting odds of the bookmaker Bet365 and evaluates them as game predictions. 


## V3_11_Evaluate_Betting_Strategies_1
This script evaluates the betting strategies described in the thesis with the models trained in *V3_09_Create_Models*. For this script the focus lays on net profit.


## V3_12_Evaluate_Betting_Strategies_2
This script too evaluates the betting strategies described in the thesis with the models trained in *V3_09_Create_Models*, but for this script the focus lays on the relative return factor.
