source('~/cuny_masters/DATA_698/data_collection_functions.R')
source('~/cuny_masters/DATA_698/secret.r')
require("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "nfldb",
                 host = "localhost", port = 5432,
                 user = user, password = pw)
## To Do:
#DONE switch out GB and NYG
#DONE trim down return dataframe
#DONE add target variable (home team win/loss) to game_play_features
#DONEwrite function to pull all non preseason, non tie games
#write code to call game_play_features() on each game from 09-16,
#and add to one big data set
#split into train/test sets
#train model
#cross validate

