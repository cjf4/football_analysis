require("RPostgreSQL")
library(stringr)

pw <- "1bomber3"

drv <- dbDriver("PostgreSQL")



con <- dbConnect(drv, dbname = "nfldb",
                 host = "localhost", port = 5432,
                 user = "christopherfenton", password = pw)

#if true, we are good to go
dbExistsTable(con, "meta")

first_plays <- dbGetQuery(con, "SELECT gsis_id FROM play_player GROUP BY gsis_id ORDER BY gsis_id LIMIT 1000 ")


game1_plays <- dbGetQuery(con, "SELECT gsis_id, 
                                       drive_id, 
                                       play_id, 
                                        time, 
                                        pos_team, 
                                        yardline,
                                        down,
                                        yards_to_go,
                                        description, timeout
                                FROM play
                                WHERE gsis_id = '2012011501' AND timeout != 1
                                ORDER BY play_id")

#build a table/df that has:


#dataframe that has home score dif
#gametime remaining
#down
#distance
#field position
#(maybe) timeouts

#example for game 2012011501

scoring_plays <- dbGetQuery(con, "SELECT gsis_id, play_id, 
                                        sum(play_player.defense_frec_tds) * 6 AS d_fum_td,
                                        sum(play_player.defense_int_tds) * 6 AS d_int_td,
                                        sum(play_player.defense_misc_tds) * 6 AS d_misc_td,
                                        sum(play_player.defense_safe) * 2 AS d_safe,
                                        sum(play_player.fumbles_rec_tds) * 6 AS fum_td,
                                        sum(play_player.kicking_fgm) * 3 AS fgm,
                                        sum(play_player.kicking_rec_tds) * 6 AS rec_td,
                                        sum(play_player.kicking_xpmade) * 1 AS xp,
                                        sum(play_player.kickret_tds) * 6 AS kick_ret_td,
                                        sum(play_player.passing_tds) * 6 AS pass_td,
                                        sum(play_player.passing_twoptm) * 2 AS pass_twopt,
                                        sum(play_player.puntret_tds) * 6 AS punt_ret_td,
                                        sum(play_player.rushing_tds) * 6 AS rush_td,
                                        sum(play_player.rushing_twoptm) * 2 AS rush_twopt 
                                FROM play_player 
                                WHERE gsis_id = '2012011501'
                                GROUP BY gsis_id, play_id
                                ")

game1_plays <- dbGetQuery(con, "SELECT  play.gsis_id, 
                                        play.drive_id, 
                                        play.play_id, 
                                        play.time, 
                                        play.pos_team, 
                                        play.yardline,
                                        play.down,
                                        play.yards_to_go,
                                        play.timeout
                                FROM play
                                WHERE play.gsis_id = '2012011501' AND 
                                      play.timeout != 1 AND 
                                      play.pos_team != 'UNK'
                                ORDER BY play.play_id")



seconds_passed <- function (gametime) {
  #requires "stringr" package
  
  #extract the quarter from gametime. if NA, assume game is over.
  quarter <- str_extract(gametime, "(?<=Q)[:digit:]{1}")
  if (is.na(quarter)) { return(3600)}
  
  #extract the seconds remaining
  seconds <- as.numeric(str_extract(gametime, "(?<=,)[:digit:]+"))
  
  #use the quarter to determine how many seconds passed in prior quarters
  quarter_list <- list(0,900,1800,2700)
  names(quarter_list) <- c("1","2","3","4")
  prev_q_seconds <- quarter_list[[quarter]]
  
  return(seconds + (prev_q_seconds))
}
g1_times <- unlist(lapply(game1_plays$time, seconds_passed))

#remove parenthesis and convert field position to numeric
field_position <- function (yardline) {
  return(as.numeric(str_extract(yardline, "[^()]+")))
}

g1_field_positions <- unlist(lapply(game1_plays$yardline, field_position))

#instatiate 0 score vectors for home and away
game1_plays$away_score <- rep(0,length(game1_plays$play_id))
game1_plays$home_score <- rep(0,length(game1_plays$play_id))


#go play by play, determine if there was a score, and increment appropriately

#list of potential scoring plays in play_player
#defense_frec_tds
#defense_int_tds
#defense_misc_tds
#defense_safe
#fumbles_rec_tds
#kicking_fgm
#kicking_rec_tds
#kicking_xpmade
#kickret_tds
#passing_tds
#passing_twoptm
#puntret_tds
#rushing_tds
#rushing_twoptm



