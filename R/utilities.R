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
  # iData : Data frame containing probability of occurrence as a variable named win_probability 
  
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
get_simulation_profit_loss <- function(iSimulation, iBets) {
  
  #profit_loss <- ifelse(
  #  dim(iSimulation)[2] > 1
  #  , colSums(iSimulation * (1 + iBets$odds) * iBets$wagered - iBets$wagered)
  #  , iSimulation * (1 + iBets$odds) * iBets$wagered - iBets$wagered
  #)
  
  if (dim(iSimulation)[2] > 1) {
    
    profit_loss <- sort(colSums(iSimulation * (1 + iBets$odds) * iBets$wagered - iBets$wagered))
    
  } else {
    
    profit_loss <- iSimulation * (1 + iBets$odds) * iBets$wagered - iBets$wagered
    
  }
  
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
  bet_input  <- left_join(games, moneylines, by=join_by(league_id, team_name))
  
  #-- Get NBA Team Names
  nba_db_data <- dbGetQuery(iDB, "select * from teams where league_id = 1;")
  
  bet_input <- attach_game_id(left_join(bet_input, nba_db_data, by=join_by(league_id, team_name))) %>%
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
  bet_input  <- left_join(games, moneylines, by=join_by(league_id, team_name))
  
  #-- Get MLB Team Names
  mlb_db_data <- dbGetQuery(iDB, "select * from teams where league_id = 2;")
  
  bet_input <- attach_game_id(left_join(bet_input, mlb_db_data, by=join_by(league_id, team_name))) %>%
    rename("team_id"="id")
  
  bet_input$odds <- convert_moneyline_to_odds(bet_input)
  bet_input$kelly_bet <- get_kelly_bet(bet_input)
  bet_input$date <- Sys.Date()
  
  bet_input <- bet_input %>% 
    relocate(game_id, league_id, team_id, date, location, team_name, win_probability, moneyline, odds, kelly_bet)
  
  return(bet_input)
  
}

#-- Get Basic NHL Daily Betting Information
get_nhl_daily_bets <- function(iDB) {
  
  #-- Needs established DB connection
  
  #-- Scrape Web Data
  games      <- get_nhl_win_probabilities()
  moneylines <- get_nhl_money_lines()
  
  #-- Reduce to correct dimensions
  nhl_min_dimension <- min(dim(moneylines)[1],dim(games)[1])
  games <- games[1:nhl_min_dimension,]
  moneylines <- moneylines[1:nhl_min_dimension,]
  
  #-- Join Probabilities and Payouts
  bet_input  <- left_join(games, moneylines, by=join_by(league_id, team_name))
  
  #-- Get MLB Team Names
  nhl_db_data <- dbGetQuery(iDB, "select * from teams where league_id = 4;")
  
  bet_input <- attach_game_id(left_join(bet_input, nhl_db_data, by=join_by(league_id, team_name))) %>%
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
  
  return(bet_xfer)
  
}


#-- Persist Daily MLB Bets to Database
store_mlb_games_to_db <- function(iDB) {
  
  #-- Get MLB Data
  bet_xfer <- get_mlb_daily_bets(iDB)
  
  #-- Write to Database
  dbWriteTable(iDB, "bets", bet_xfer, append=TRUE, row.names=FALSE)
  
  return(bet_xfer)
  
}


#-- Persist Daily NHL Bets to Database
store_nhl_games_to_db <- function(iDB) {
  
  #-- Get MLB Data
  bet_xfer <- get_nhl_daily_bets(iDB)
  
  #-- Write to Database
  dbWriteTable(iDB, "bets", bet_xfer, append=TRUE, row.names=FALSE)
  
  return(bet_xfer)
  
}

#-- Read Potential bets from database for simulating a betting day
get_simulated_bets <- function(iDB, iLeague, iDay) {
  
  sql <- paste("select number_of_games from games_calendar where league_id =", iLeague,  "and day_of_year = ", iDay)
  
  number_of_games <- as.numeric(dbGetQuery(iDB, sql))
  
  #-- Sample from bets table and return
  
  return(number_of_games)
  
}

#-- Determine Kelly Bet of a simulated profit loss distribution
get_kelly_bet_from_distribution <- function(iDistribution) {
  
  #-- iDistribution is single column vector of profit and loss
  pnl <- data.frame(table(iDistribution))
  pnl[,1] <- as.numeric(as.character(pnl[,1]))
  
  number_of_wins <- sum(pnl[pnl[,1] > 0, 2])
  number_of_sims <- sum(pnl$Freq)
  
  win_probability <- number_of_wins / number_of_sims
  
  wins <- pnl[pnl[,1] > 0,]
  
  wins$conditional_probability <- wins$Freq / number_of_wins
  
  win_odds <- 1 + sum(wins[,1] * wins$conditional_probability) / win_probability
  
  kelly_bet_from_distribution <- (win_odds * win_probability - (1 - win_probability) ) / win_odds
  
  return(kelly_bet_from_distribution)
  
}

#-- Get Returns from Daily Bets
get_bet_returns <- function(iBets) {
  
  #Requires iBets to include following fields: wagered, odds, win
  
  returned <- iBets$wagered * (1 + iBets$odds) * iBets$win
  
  return(returned)
  
}

#-- Get Profit/Loss from Daily Bets
get_profit_loss <- function(iBets) {
  
  #Requires iBets to include following fields: wagered, odds, win
  profit_loss <- sum(get_bet_returns(iBets) - iBets$wagered)
  
  return(profit_loss)
  
}

#-- Get Return on Wager from Daily Bets
get_return_on_wager <- function(iBets) {
  
  #Requires iBets to include following fields: wagered, odds, win
  return_on_wager <- get_profit_loss(iBets) / sum(bets$wagered)
  
  return(return_on_wager)
  
}

#-- Get Estimated Return Percentile of Expected
get_estimated_return_percentile <- function(iBets) {
  
  sim_results <- get_simulation_outcomes(1E5, 0.025, iBets)
  sim_profit_loss <- get_simulation_profit_loss(sim_results, iBets)
  estimated_percentile_values <- unname(quantile(sim_profit_loss, seq(0,1,0.001)))
  percentiles <- seq(0,1,0.001)
  profit_loss <- get_profit_loss(iBets)
  
  estimated_return_percentile <- min(percentiles[estimated_percentile_values > get_profit_loss(iBets)])
  
  return(estimated_return_percentile)
  
}