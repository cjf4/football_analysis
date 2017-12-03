require("RPostgreSQL")

pw <- "1bomber3"

drv <- dbDriver("PostgreSQL")



con <- dbConnect(drv, dbname = "nfldb",
                 host = "localhost", port = 5432,
                 user = "christopherfenton", password = pw)

#if true, we are good to go
dbExistsTable(con, "meta")




