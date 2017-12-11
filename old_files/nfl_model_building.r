library(ggplot2)
library(dplyr)
library(randomForest)
library(tree)

nfl_data <- read.csv("game_data.csv")

filter(nfl_data, is.na(home_score))
nfl_data <- nfl_data[-114849,]
nfl_data[is.na(nfl_data)] <- 0
unlist(lapply(nfl_data, function(x) any(is.na(x))))

nfl_data$home_posession <- nfl_data$pos_team == nfl_data$home_team
nfl_data$home_posession <- nfl_data$home_possession * 1




train <- filter(nfl_data, season_year != 2015)

formula <- home_win ~ down+ yards_to_go+ game_secs+ field_pos+ home_score+ away_score+ home_posession

tree_model <- tree(formula, data = train)

