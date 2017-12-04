source('~/cuny_masters/DATA_698/data_collection_functions.R')
source('~/cuny_masters/DATA_698/secret.r')
require("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "nfldb",
                 host = "localhost", port = 5432,
                 user = user, password = pw)

game_list <- game_list_query(con)

game_data <- game_play_features(con, game_list$gsis_id[1])

for (i in 2:length(game_list$gsis_id)) {
  game_data <- rbind(game_data, game_play_features(con, game_list$gsis_id[i]))
}

x <- length(game_list$gsis_id)
#DEBUGGING NA issue with scoring values
#game_id <- 2017010115

#scoring_plays <- scoring_plays(con, game_id)
#game_plays <- game_plays(con, game_id )
#drives <- drives(con, game_id)
#game_info <- game_info(con, game_id)

#plays <- game_plays %>%
#  left_join(scoring_plays)

#plays <- scoring_NA_to_0(plays)

## To Do:
#DONE switch out GB and NYG
#DONE trim down return dataframe
#DONE add target variable (home team win/loss) to game_play_features
#DONE write function to pull all non preseason, non tie games
#DONE write code to call game_play_features() on each game from 09-16,
#DONE and add to one big data set
#add game demographic info from game info so that the data 
      #can be subsetted by year season type, etc
#do some visualizations
#split into train/test sets
#train model
#cross validate
#add a readme
