require("RPostgreSQL")

pw <- "password"

drv <- dbDriver("PostgreSQL")



con <- dbConnect(drv, dbname = "nfldb",
                 host = "localhost", port = 5432,
                 user = "user", password = pw)

#if true, we are good to go
dbExistsTable(con, "meta")




