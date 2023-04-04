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
get_simulation_outcomes <- function(iNumberSimulations, iVaryProbability, iData) {
  
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
