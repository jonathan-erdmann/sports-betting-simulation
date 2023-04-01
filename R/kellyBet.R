#-- Return Kelly Bet sizing
get_kelly_bet <- function(iData) {
 
  #-- iData data frame must include the following fields
  #    chance: Probability of win
  # moneyline: The money line associated with the team win
  
  p  <- iData$chance
  ml <- iData$moneyline
  
  q <- 1 - p
  odds <- ifelse(ml > 0, ml / 100, -100 / ml)
  
  kelly_bet <- (odds * p - q) / odds
  kelly_bet <- ifelse(kelly_bet > 0, kelly_bet, 0)
  
  return(kelly_bet)
  
}