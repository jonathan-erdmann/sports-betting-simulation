#-- Utility Functions for Betting Simulation

#-- Return Kelly Bet sizing
get_kelly_bet <- function(iData) {
  
  #-- iData data frame must include the following fields
  # win_probability : Probability of win
  #       moneyline : The money line associated with the team win
  #
  # Input  : Probability of win and betting odds
  # Output : Kelly bet size
  
  p  <- iData$win_probability
  ml <- iData$moneyline
  
  q <- 1 - p
  odds <- ifelse(ml > 0, ml / 100, -100 / ml)
  
  kelly_bet <- (odds * p - q) / odds
  kelly_bet <- ifelse(kelly_bet > 0, kelly_bet, 0)
  
  return(kelly_bet)
  
}

#-- Return Implied Probability of a Money Line
convert_moneyline_to_probability <- function(iData) {
  
  #-- iData data frame must include the following fields
  # moneyline: The moneyline associated with a bet
  #
  # Input  : Moneyline
  # Output : Implied fair game probability
  
  moneyline <- iData$moneyline
  probability <- ifelse(moneyline >= 100, moneyline / 100,
                          ifelse(moneyline <= -100, -100 / moneyline, NA)
                          )
  probability <- 1 / (1 + probability)
  
  return(probability)
  
}

#-- Return a matrix of simulated wins and losses given probabilities
get_simulation_outcomes <- function(iNumberSimulations, iVaryProbability = 0.025, iData) {
  
  #-- Inputs:
  # iNumberSimulations : Integer of number of simulations to return
  # iVaryProbability   : Standard deviation of probability point estimates to use on each simulation
  # iData : Data frame containing probability of occurrence as a variable named chance 
  
  #-- Output:
  # simulation_outcomes : Number of Games matrix by iNumberSimulations of 0-Losses and 1-Wins
  
  #-- Generate simulation probabilities
  #Repeat point estimates
  simulation_probabilities <- matrix(rep(iData$win_probability, iNumberSimulations), nrow=length(iData$win_probability))
  
  #Add variability to point estimate if input
  simulation_probabilities <- simulation_probabilities + rnorm(prod(dim(simulation_probabilities)), 0, iVaryProbability)
  
  #Simulated Outcomes
  simulated_outcomes <- matrix(runif(prod(dim(simulation_probabilities))), nrow=length(iData$win_probability))
  
  #Simulated Wins
  simulated_wins <- ifelse(simulation_probabilities > simulated_outcomes, 1, 0)
  
  return(simulated_wins)
  
}

#-- Convert moneyline into payout odds
convert_moneyline_to_odds <- function(iData) {
  
  moneyLine <- iData$moneyline
  odds <- ifelse(moneyLine > 0, moneyLine / 100, -100 / moneyLine)
  
  return(odds)
  
}

#-- Return Profit/Loss Distribution
get_profit_loss <- function(iSimulation, iBets) {
  
  profit_loss <- ifelse(
    dim(iSimulation)[2] > 1
    , colSums(iSimulation * (1 + iBets$odds) * iBets$wagered - iBets$wagered)
    , iSimulation * (1 + iBets$odds) * iBets$wagered - iBets$wagered
  )
  
  profit_loss <- sort(profit_loss)
  
  return(profit_loss)

}

#-- Attach Game ID to Bet Input Data Frame
attach_game_id <- function(iBets) {
  
  game_id <- 1:length(iBets$league)
  game_id <- paste0(ceiling(game_id / 2), Sys.Date())
  game_id <- sapply(game_id, digest)
  
  iBets$game_id <- game_id
  
  return(iBets)
  
}

#-- Get Basic NBA Daily Betting Information
get_nba_daily_bets <- function(iDB) {
  
  #-- Needs established DB connection
  
  #-- Scrape Web Data
  games      <- get_nba_win_probabilities()
  moneylines <- get_nba_money_lines()
  bet_input  <- left_join(games, moneylines, by=join_by(league, team_name))
  
  #-- Get NBA Team Names
  nba_db_data <- dbGetQuery(iDB, "select * from teams where league_id = 1;")
  
  bet_input <- attach_game_id(left_join(bet_input, nba_db_data, by=join_by(team_name))) %>%
    select(-c("league")) %>%
    rename("team_id"="id")
  
  bet_input$odds <- convert_moneyline_to_odds(bet_input)
  bet_input$kelly_bet <- get_kelly_bet(bet_input)
  bet_input$date <- Sys.Date()
  
  bet_input <- bet_input %>% 
    relocate(game_id, league_id, team_id, date, location, team_name, win_probability, moneyline, odds, kelly_bet)
  
  return(bet_input)
  
}

#-- Get Basic MLB Daily Betting Information
get_mlb_daily_bets <- function(iDB) {
  
  #-- Needs established DB connection
  
  #-- Scrape Web Data
  games      <- get_mlb_win_probabilities()
  moneylines <- get_mlb_money_lines()
  
  #-- Reduce to correct dimensions
  number_mlb_games <- dim(games)[1]
  moneylines <- moneylines[1:number_mlb_games,]

  #-- Join Probabilities and Payouts
  bet_input  <- left_join(games, moneylines, by=join_by(league, team_name))
  
  #-- Get MLB Team Names
  mlb_db_data <- dbGetQuery(iDB, "select * from teams where league_id = 2;")
  
  bet_input <- attach_game_id(left_join(bet_input, mlb_db_data, by=join_by(team_name))) %>%
    select(-c("league")) %>%
    rename("team_id"="id")
  
  bet_input$odds <- convert_moneyline_to_odds(bet_input)
  bet_input$kelly_bet <- get_kelly_bet(bet_input)
  bet_input$date <- Sys.Date()
  
  bet_input <- bet_input %>% 
    relocate(game_id, league_id, team_id, date, location, team_name, win_probability, moneyline, odds, kelly_bet)
  
  return(bet_input)
  
}

#-- Persist Daily NBA Bets to Database
store_nba_games_to_db <- function(iDB) {

  #-- Get NBA Data
  bet_xfer <- get_nba_daily_bets(iDB)
  
  #-- Write to Database
  dbWriteTable(iDB, "bets", bet_xfer, append=TRUE, row.names=FALSE)
  
}


#-- Persist Daily MLB Bets to Database
store_mlb_games_to_db <- function(iDB) {
  
  #-- Get MLB Data
  bet_xfer <- get_mlb_daily_bets(iDB)
  
  #-- Write to Database
  dbWriteTable(iDB, "bets", bet_xfer, append=TRUE, row.names=FALSE)
  
}

#-- Read Potential bets from database for simulating a betting day
get_simulated_bets <- function(iDB, iLeague, iDay) {
  
  sql <- paste("select number_of_games from games_calendar where league_id =", iLeague,  "and day_of_year = ", iDay)
  
  number_of_games <- as.numeric(dbGetQuery(iDB, sql))
  
  #-- Sample from bets table and return
  
  return(number_of_games)
  
}