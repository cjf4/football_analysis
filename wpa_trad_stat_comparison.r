source('~/cuny_masters/DATA_698/data/secret.r')
require("RPostgreSQL")
require("dplyr")
require("psych")
require("reshape")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "nfldb",
                 host = "localhost", port = 5432,
                 user = user, password = pw)

nfl_data <- read.csv("data/nfl_data_wpa.csv")


player_stats <- dbGetQuery(con,
                           "SELECT
                           play_player.gsis_id,
                           play_player.play_id,
                           play_player.player_id,
                           play_player.fumbles_lost,
                           play_player.passing_att,
                           play_player.passing_cmp,
                           play_player.passing_int,
                           play_player.passing_sk_yds,
                           play_player.passing_sk,
                           play_player.passing_tds,
                           play_player.passing_yds,
                           play_player.rushing_att,
                           play_player.rushing_loss,
                           play_player.rushing_loss_yds,
                           play_player.rushing_tds,
                           play_player.rushing_yds,
                           play_player.receiving_rec,
                           play_player.receiving_tar,
                           play_player.receiving_tds,
                           play_player.receiving_yac_yds,
                           play_player.receiving_yds
                           FROM play_player")

player_stats$gsis_id <- as.integer(player_stats$gsis_id)

all_data <- left_join(nfl_data, player_stats)

#all_data$gsis_id <- as.character(all_data$gsis_id)
#all_data$player_id <- as.character(all_data$player_id)

passer_rating_comparison <- all_data %>%
                              select(gsis_id,
                                     wpa,
                                     player_id,
                                     passing_cmp,
                                     passing_att,
                                     passing_yds,
                                     passing_int,
                                     passing_tds) %>%
                              filter(passing_att > 0) %>% 
                              group_by(gsis_id, player_id) %>%
                              summarise(total_wpa = sum(wpa),
                                        passing_cmp = sum(passing_cmp),
                                        passing_att = sum(passing_att),
                                        passing_yds = sum(passing_yds),
                                        passing_int = sum(passing_int),
                                        passing_tds = sum(passing_tds))

#player names

players <- dbGetQuery(con,
           "SELECT
              player_id,
              full_name,
              team,
              position,
              years_pro
            FROM
              player")

passer_rating <- function(game) {
  comp <- game$passing_cmp
  yds <- game$passing_yds
  att <- game$passing_att
  td <- game$passing_tds
  int <- game$passing_int
  a <- (comp/att - .3)* 5
  b <- (yds/att - 3) * .25
  c <- (td/att) * 20
  d <- 2.375 - (int/att * 25)
  return(((a+b+c+d)/6)*100)
}

passer_rating_comparison$passer_rating <- rep(0, nrow(passer_rating_comparison))

for (i in 1:nrow(passer_rating_comparison)) {
  passer_rating_comparison$passer_rating[i] <- passer_rating(passer_rating_comparison[i,])
}

passer_stats <-left_join(passer_rating_comparison, players)

passer_st_team <- filter(passer_stats, passing_att >4, team != "UNK")


#need to get the result of each game

game_frame <- nfl_data %>%
                  select(gsis_id, home_win, home_team, away_team, season_year) %>%
                  group_by(gsis_id, home_team, away_team, season_year) %>%
                  summarise(home_winner = mean(home_win))

passer_st_team <- left_join(passer_st_team, game_frame)

passer_st_team$win <- ifelse((passer_st_team$team == passer_st_team$home_team) & (passer_st_team$home_winner ==1),
                          1,
                         ifelse((passer_st_team$team == passer_st_team$home_team) & (passer_st_team$home_winner ==0),
                                0,
                                ifelse((passer_st_team$team == passer_st_team$away_team) & (passer_st_team$home_winner ==1),
                                       0,
                                       ifelse((passer_st_team$team == passer_st_team$away_team) & (passer_st_team$home_winner ==0),
                                              1,
                                              NA))))

passer_st_team <- na.omit(passer_st_team)


comparison_frame <- select(passer_st_team, passer_rating, total_wpa, win)

cor(comparison_frame$total_wpa, comparison_frame$passer_rating)

#scale and center the distributions
scaled <- scale(comparison_frame[,2:3])
cor(scaled[,1], scaled[,2])

comparison_frame$scaled_wpa <- scaled[,1]
comparison_frame$scaled_pr <- scaled[,2]

#distributions are properly scaled
density_data <- melt(comparison_frame[,5:6])
ggplot(density_data, aes(x=value, fill= variable)) + geom_density(alpha = .2)

grouped_cf <-- comparison_frame %>%
                select(win, scaled_wpa, scaled_pr) %>%
                group_by(win) %>%
                summarise(mean_scaled_wpa = mean(scaled_wpa),
                          mean_scaled_pr = mean(scaled_pr))

table(comparison_frame$win, mean(comparison_frame$scaled_wpa))

write.csv(comparison_frame, "data/comparison_frame.csv")
write.csv(passer_st_team, "data/passer_rating_data.csv")