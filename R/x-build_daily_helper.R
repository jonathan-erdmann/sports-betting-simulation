library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(DBI)
library(RSQLite)
library(data.table)
library(digest)
library(ggthemes)

dbFileName <- "bettingSimulation.sqlite"
dbPath <- file.path("C:","Users","Jonathan","Documents","Jonathan","r-projects","sports-betting-simulation", "db")
dbFile <- paste0(dbPath,"/" ,dbFileName)

#Connect to database
mydb <- dbConnect(RSQLite::SQLite(), dbFile)

#-- Store all betting information
#mlb_games <- store_mlb_games_to_db(mydb)
#nba_games <- store_nba_games_to_db(mydb)
#nhl_games <- store_nhl_games_to_db(mydb)

#-- Find favorable bets
today <- as.numeric(Sys.Date())
query <- paste("select * from bets where kelly_bet > 1E-6 and date = ", today)
bets  <- dbGetQuery(mydb, query) %>% arrange(-kelly_bet)

#-- Develop PNL distribution based on scaled wager
bets$wagered    <- bets$kelly_bet / sum(bets$kelly_bet)
sim_results     <- get_simulation_outcomes(10E4, 0.025, bets)
sim_pnl         <- get_profit_loss(sim_results, bets)
total_kelly_bet <- get_kelly_bet_from_distribution(sim_pnl)

#-- Wager 1/2 Size Kelly Bet
bet_factor <- 0.5
bets$wagered <- bet_factor * total_kelly_bet * bets$wagered
sim_pnl <- get_profit_loss(sim_results, bets)

pnl_plot_data <- data.frame(table(round(sim_pnl,3))) %>% mutate(prob=Freq/sum(Freq), cum_prob=cumsum(prob)) %>% rename("profit"="Var1")
pnl_plot_data$profit <- as.numeric(as.character(pnl_plot_data$profit))
pnl_plot_data$win <- pnl_plot_data$profit > 0

total_wagered <- sum(bets$wagered)
loss_probability <- mean(sim_pnl <0)
title_text <- paste0("Expected Cumulative Profit/Loss Distribution\n\tTotal Wagered: ", round(total_wagered,3), "\n\tProbability of Loss: ", round(100*loss_probability,2), "%")

g <- ggplot(pnl_plot_data, aes(x = profit, y = cum_prob)) + geom_line() + geom_point(aes(color=win))
g <- g + ggtitle(title_text)
g <- g + xlab("Profit") + ylab("Cumulative Probability") + theme_pander()
g <- g + xlim(c(-0.5, 0.5))
g

title_text <- paste0("Expected Profit/Loss Distribution\n\tTotal Wagered: ", round(total_wagered,3), "\n\tProbability of Loss: ", round(100*loss_probability,2), "%")

g <- ggplot(pnl_plot_data, aes(x = profit, y = prob)) + geom_point() + geom_line()
g <- g + ggtitle(title_text)
g <- g + xlab("Profit") + ylab("Cumulative Probability") + theme_pander()
g <- g + xlim(c(-0.5, 0.5))
g

dbDisconnect(mydb)
