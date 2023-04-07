library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(DBI)
library(RSQLite)
library(data.table)
library(digest)

nba_games      <- get_nba_win_probabilities()
nba_moneylines <- get_nba_money_lines()
nba_bet_input  <- left_join(nba_games, nba_moneylines, by=join_by(league, team_name))
kelly_bet      <- get_kelly_bet(nba_bet_input)

nba_bets <- data.frame(nba_bet_input, kelly_bet) %>% filter(kelly_bet > 0)
nba_bets$odds <- convert_moneyline_to_odds(nba_bets)

mlb_games        <- get_mlb_win_probabilities()
mlb_moneylines   <- get_mlb_money_lines()
number_mlb_games <- dim(mlb_games)[1]
mlb_moneylines   <- mlb_moneylines[1:number_mlb_games,]
mlb_bet_input    <- left_join(mlb_games, mlb_moneylines, by=join_by(league, team_name))
kelly_bet        <- get_kelly_bet(mlb_bet_input)

mlb_bets <- data.frame(mlb_bet_input, kelly_bet) %>% filter(kelly_bet > 0)
mlb_bets$odds <- convert_moneyline_to_odds(mlb_bets)

bets <- rbind(nba_bets, mlb_bets)
bets <- bets %>% mutate(kelly_bet_scaled = kelly_bet / sum(kelly_bet)) %>% arrange(-kelly_bet)

dbDisconnect(mydb)