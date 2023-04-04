library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(DBI)
library(RSQLite)
library(data.table)

nba_games      <- get_nba_win_probabilities()
nba_moneylines <- get_nba_money_lines()
nba_bet_input  <- left_join(nba_games, nba_moneylines, by=join_by(league, team_name))
kelly_bet      <- get_kelly_bet(nba_bet_input)

nba_bets <- data.frame(nba_bet_input, kelly_bet) %>% filter(kelly_bet > 0)

mlb_games        <- get_mlb_win_probabilities()
mlb_moneylines   <- get_mlb_money_lines()
number_mlb_games <- dim(mlb_games)[1]
mlb_moneylines   <- mlb_moneylines[1:number_mlb_games,]
mlb_bet_input    <- left_join(mlb_games, mlb_moneylines, by=join_by(league, team_name))
kelly_bet        <- get_kelly_bet(mlb_bet_input)

mlb_bets <- data.frame(mlb_bet_input, kelly_bet) %>% filter(kelly_bet > 0)

bets <- rbind(nba_bets, mlb_bets)
bets <- bets %>% 
  mutate(kelly_bet_scaled = kelly_bet / sum(kelly_bet), kelly_bet_qtr = kelly_bet_scaled / 4) %>%
  arrange(-kelly_bet)

#-- Store Available Bets into Database
dbFileName <- "bettingSimulation.sqlite"
dbPath <- file.path("C:","Users","Jonathan","Documents","Jonathan","r-projects","sports-betting-simulation", "db")
dbFile <- paste0(dbPath,"/" ,dbFileName)

today <- Sys.Date()

mydb <- dbConnect(RSQLite::SQLite(), dbFile)

#-- Get MLB Team Names
mlb_db_data <- dbGetQuery(mydb, "select * from teams where league_id = 2;")

#-- Attach Team ID to mlb_bet_input data frame
mlb_bet_xfer <- left_join(mlb_bet_input, mlb_db_data, by=join_by(team_name)) %>% select(league_id, id, win_probability, moneyline)
date <- rep(Sys.Date(),dim(mlb_bet_xfer)[1])
mlb_bet_xfer <- data.frame(mlb_bet_xfer, date) %>% select(league_id, date, id, win_probability, moneyline)
mlb_bet_xfer <- mlb_bet_xfer %>% rename("team_id" = "id")

#-- Get NBA Team Names
nba_db_data <- dbGetQuery(mydb, "select * from teams where league_id = 1;")

#-- Attach Team ID to mlb_bet_input data frame
nba_bet_xfer <- left_join(nba_bet_input, nba_db_data, by=join_by(team_name)) %>% select(league_id, id, win_probability, moneyline)
date <- rep(Sys.Date(),dim(nba_bet_xfer)[1])
nba_bet_xfer <- data.frame(nba_bet_xfer, date) %>% select(league_id, date, id, win_probability, moneyline)
nba_bet_xfer <- nba_bet_xfer %>% rename("team_id" = "id")

#-- Write to DB
dbWriteTable(mydb, "bets", mlb_bet_xfer, append=TRUE, row.names=FALSE)
dbWriteTable(mydb, "bets", nba_bet_xfer, append=TRUE, row.names=FALSE)

dbDisconnect(mydb)