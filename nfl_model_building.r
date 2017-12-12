library(ggplot2)
library(dplyr)
library(randomForest)
library(tree)

nfl_data <- read.csv("data/game_data.csv", stringsAsFactors = FALSE)

filter(nfl_data, is.na(home_score))
nfl_data <- nfl_data[-114849,]
nfl_data[is.na(nfl_data)] <- 0
unlist(lapply(nfl_data, function(x) any(is.na(x))))

nfl_data$home_posession <- nfl_data$pos_team == nfl_data$home_team
nfl_data$home_posession <- nfl_data$home_posession * 1


offense_win <- function(play) {
  if (play$pos_team == play$home_team) {
    return(ifelse(play$home_win == 1,1,0))
  } else {
    return(ifelse(play$home_win == 0,1,0))
  } 
}

nfl_data$offense_win <- rep(0, length(nfl_data$X))

nfl_data$offense_win <- ifelse(nfl_data$pos_team == nfl_data$home_team,
                                ifelse(nfl_data$home_win == 1,1,0),
                                ifelse(nfl_data$home_win == 0,1,0))


#for (i in 1:length(nfl_data[,1])) {
#  nfl_data$offense_win[i] <- offense_win(nfl_data[i,])
#}

train <- filter(nfl_data, season_year != 2015)
test <- filter(nfl_data, season_year == 2016)

offense_win <- offense_win ~ down+ yards_to_go+ game_secs+ field_pos+ home_score+ away_score+ home_posession
home_win <- home_win ~ down+ yards_to_go+ game_secs+ field_pos+ home_score+ away_score+ home_posession

tree_model <- tree(offense_win, data = train)

binlog_model <- glm(offense_win, data = train, family = binomial(link="logit"))
