library(ggplot2)
library(dplyr)
library(randomForest)
library(tree)
library(caret)

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

nfl_data$offense_score <- rep(0, length(nfl_data$X))
nfl_data$defense_score <- rep(0, length(nfl_data$X))

nfl_data$offense_score <- ifelse(nfl_data$pos_team == nfl_data$home_team,
                                    nfl_data$home_score,
                                    nfl_data$away_score)

nfl_data$defense_score <- ifelse(nfl_data$pos_team == nfl_data$home_team,
                                 nfl_data$away_score,
                                 nfl_data$home_score)

nfl_data_reduced <- nfl_data[,c("pos_team",
                                "down",
                                "yards_to_go",
                                "game_secs",
                                "field_pos",
                                "season_year",
                                "offense_score",
                                "defense_score",
                                "offense_win")]

nfl_data_reduced$offense_win_factor <- factor(nfl_data_reduced$offense_win)


train <- filter(nfl_data_reduced, season_year %in% c(2013,2014,2015))
test <- filter(nfl_data_reduced, season_year == 2016)

#baseline, where we always predict the winning team to win
baseline_train <- filter(train, offense_score != defense_score)

offense_wins_correct <- nrow(filter(baseline_train, offense_win == 1, 
                                    offense_score > defense_score))

defense_wins_correct <- nrow(filter(baseline_train, offense_win == 0, 
                                    offense_score < defense_score))


#decision tree features
offense_win_formula <- offense_win_factor ~ down + 
                                            yards_to_go + 
                                            game_secs + 
                                            field_pos + 
                                            offense_score + 
                                            defense_score 

offense_win_regression <- offense_win ~ down + 
                          yards_to_go + 
                          game_secs + 
                          field_pos + 
                          offense_score + 
                          defense_score 

#simple decision tree for illustration
tree_model <- tree(offense_win_formula, data = train)
plot(tree_model)
text(tree_model)


#random forest, m = 2
rf10 <- randomForest(offense_win_formula, data=train, ntree=10)
rf10_err_rate <- tail(rf$err.rate[,1], n=1)


rf45 <- randomForest(offense_win_formula, data=train, ntree=45)
rf45_err_rate <- tail(rf$err.rate[,1], n=1)

rf50 <- randomForest(offense_win_formula, data=train, ntree=50)
rf50_err_rate <- tail(rf$err.rate[,1], n=1)

rf100 <- randomForest(offense_win_formula, data=train, ntree=100)
rf100_err_rate <- tail(rf$err.rate[,1], n=1)

rf500 <- randomForest(offense_win_formula, data=train, ntree=500)
rf500_err_rate <- tail(rf$err.rate[,1], n=1)

rf250 <- randomForest(offense_win_formula, data=train, ntree=250)
rf25 <- randomForest(offense_win_formula, data=train, ntree=25)

#bagging (m = 6)
b10 <- randomForest(offense_win_formula, data=train, ntree=10, mtry = 6)
b10_err_rate <- tail(rf$err.rate[,1], n=1)


b45 <- randomForest(offense_win_formula, data=train, ntree=45, mtry = 6)
b45_err_rate <- tail(rf$err.rate[,1], n=1)

b50 <- randomForest(offense_win_formula, data=train, ntree=50, mtry = 6)
b50_err_rate <- tail(rf$err.rate[,1], n=1)

b100 <- randomForest(offense_win_formula, data=train, ntree=100, mtry=6)
b100_err_rate <- tail(rf$err.rate[,1], n=1)

b500 <- randomForest(offense_win_formula, data=train, ntree=500, mtry=6)
b500_err_rate <- tail(rf$err.rate[,1], n=1)


b250 <- randomForest(offense_win_formula, data=train, ntree=250, mtry=6)
b25 <- randomForest(offense_win_formula, data=train, ntree=25, mtry=6)
rf250 <- randomForest(offense_win_formula, data=train, ntree=250)
rf25 <- randomForest(offense_win_formula, data=train, ntree=25)




y <- train[,"offense_win"]
x <- train [,c(2,3,4,5,7,8)]


rf10r <- randomForest(x=x,y=y, ntree=10)
rf20r <- randomForest(x=x,y=y, ntree=20)
rf30r <- randomForest(x=x,y=y, ntree=30)
rf40r <- randomForest(x=x,y=y, ntree=40)
rf50r <- randomForest(x=x,y=y, ntree=50)



predicted_rf10r <- predict(rf10r, test, type= "response")
predicted_rf50r <- predict(rf50r, test, type= "response")

test$wp <- predicted_rf50r
cut(test$wp, 20)

binned_rf50r <- test %>%
                  select(wp_bin, offense_win) %>%
                  group_by(wp_bin) %>%
                  summarise(observed_wp = mean(offense_win))

plot(binned_rf50r)

rf10r.3 <- randomForest(x=x,y=y, ntree=10)
rf20r.3 <- randomForest(x=x,y=y, ntree=20)
rf30r.3 <- randomForest(x=x,y=y, ntree=30)
rf40r.3 <- randomForest(x=x,y=y, ntree=40)
rf50r.3 <- randomForest(x=x,y=y, ntree=50)


predicted_rf50r.3 <- predict(rf50r.3, test, type= "response")

test$wp <- predicted_rf50r.3
test$wp_bin <- cut(test$wp, 20)

binned_rf50r.3 <- test %>%
  select(wp_bin, offense_win) %>%
  group_by(wp_bin) %>%
  summarise(observed_wp = mean(offense_win))
write.csv(binned_rf50r.3[,2], "3_year_data.csv")

log_reg <- glm(offense_win_regression, data = train, family = binomial(link = "logit") )

test$log_wp <- predict(log_reg, test, type= "response")
test$log_wp_bin <- cut(test$log_wp, 20)

binned_log_reg <- test %>%
  select(log_wp_bin, offense_win) %>%
  group_by(log_wp_bin) %>%
  summarise(observed_wp = mean(offense_win))

write.csv(binned_log_reg[,2], "3_year_data_log_reg.csv")

saveRDS()
