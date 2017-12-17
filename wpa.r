library(ggplot2)
library(dplyr)

load("logistic_regression_model")

nfl_data <- read.csv("data/game_data.csv", stringsAsFactors = FALSE)

#######MOVE TO DATA COLLECTION
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

nfl_data$offense_score <- rep(0, length(nfl_data$X))
nfl_data$defense_score <- rep(0, length(nfl_data$X))

nfl_data$offense_score <- ifelse(nfl_data$pos_team == nfl_data$home_team,
                                 nfl_data$home_score,
                                 nfl_data$away_score)

nfl_data$defense_score <- ifelse(nfl_data$pos_team == nfl_data$home_team,
                                 nfl_data$away_score,
                                 nfl_data$home_score)
#############MOVE TO DATA COLLECTION

#looks for a single row of a nfl play data frame
#expects to be run in the context of the same game
change_of_pos <- function(play, next_play) {
  if (play$pos_team == next_play$pos_team ){
    return(FALSE) 
  } else {
    return(TRUE)
  }
}


wp <- predict(log_reg, select(nfl_data, offense_win,
                                        yards_to_go,
                                        down,
                                        game_secs,
                                        field_pos,
                                        offense_score,
                                        defense_score),
              type="response")


nfl_data$wp <- wp

game_list <- unique(nfl_data$gsis_id)

nfl_data$wpa <- rep(0, nrow(nfl_data))

x<-0

for (i in 1:length(game_list)) {
  game <- filter(nfl_data, gsis_id == game_list[i])
  arrange(game, play_id)
  last_play <- max(game$play_id)
  for (j in 1:length(game$gsis_id)) {
    x <- x+1
    if(game$play_id[j] == last_play) {
      nfl_data$wpa[x] <- 0
    } else {
      if (x = 1){
        
      } else {
      
      play_wp <- game$wp[j]
      next_play_wp <- game$wp[j+1]
      }
    }
  }
}



for (i in 1:nrow(nfl_data)) {
  if ( nfl_data$gsis_id[i] == nfl_data$gsis_id[i + 1]){
    if(change_of_pos(nfl_data[i,],nfl_data[i+1,])) {
      nfl_data$wpa[i] <- (1-nfl_data$wp[i+1]) - nfl_data$wp[i]
    } else {
      nfl_data$wpa[i] <- nfl_data$wp[i+1] - nfl_data$wp[i] 
    }
    
  }  else  {
    nfl_data$wpa[i] <- 0
  }
}


write.csv(nfl_data,"data/nfl_data_wpa.csv")


