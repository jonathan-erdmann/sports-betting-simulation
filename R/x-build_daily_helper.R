library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(DBI)
library(RSQLite)
library(data.table)
library(digest)

dbFileName <- "bettingSimulation.sqlite"
dbPath <- file.path("C:","Users","Jonathan","Documents","Jonathan","r-projects","sports-betting-simulation", "db")
dbFile <- paste0(dbPath,"/" ,dbFileName)

#Connect to database
mydb <- dbConnect(RSQLite::SQLite(), dbFile)

mlb_games <- store_mlb_games_to_db(mydb)
nba_games <- store_nba_games_to_db(mydb)
nhl_games <- store_nhl_games_to_db(mydb)

dbDisconnect(mydb)