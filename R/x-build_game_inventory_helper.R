#-- Build Calendar with Number of Games for Simulation Inventory
offset <- 458:94
calendar_date <- Sys.Date() - offset

nba_games <- rep(0,length(calendar_date))
mlb_games <- rep(0,length(calendar_date))
nfl_games <- rep(0,length(calendar_date))
nhl_games <- rep(0,length(calendar_date))

for (ii in 1:length(calendar_date)) {
  
  temp <- get_nba_scores(offset[ii])
  
    if ( class(temp) == "data.frame" ) {
      
      nba_games[ii] <- sum(temp$win) 
      
    }
  
  Sys.sleep(sample(10, 1) * 0.1)
  
  temp <- get_mlb_scores(offset[ii])
  
    if ( class(temp) == "data.frame" ) {
      
      mlb_games[ii] <- sum(temp$win) 
      
    }
  
  Sys.sleep(sample(10, 1) * 0.1)
  
  temp <- get_nhl_scores(offset[ii])
  
    if ( class(temp) == "data.frame" ) {
      
      nhl_games[ii] <- sum(temp$win) 
      
    }
  
  Sys.sleep(sample(10, 1) * 0.1)
  
  print(paste(calendar_date[ii], nba_games[ii], mlb_games[ii], nhl_games[ii]))
    
  #nfl_games[ii] <- dim(get_nfl_scores(offset[ii]))[1] / 2
  
}

game_inventory <- data.frame(calendar_date, nba_games, mlb_games, nhl_games)
