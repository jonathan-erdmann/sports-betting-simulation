#-- Utility Functions for Betting Simulation

#-- Return Kelly Bet sizing
get_kelly_bet <- function(iData) {
  
  #-- iData data frame must include the following fields
  #    chance: Probability of win
  # moneyline: The money line associated with the team win
  #
  # Input  : Probability of win and betting odds
  # Output : Kelly bet size
  
  p  <- iData$chance
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