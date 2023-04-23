#-- Run a Betting Simulation

#-- Randomize Start Day
start_day <- round(runif(1, min = 1, max = 364))

#-- Connect to Database
dbFileName <- "bettingSimulation.sqlite"
dbPath <- file.path("C:","Users","Jonathan","Documents","Jonathan","r-projects","sports-betting-simulation", "db")
dbFile <- paste0(dbPath,"/" ,dbFileName)
mydb <- dbConnect(RSQLite::SQLite(), dbFile)

#-- Read Simulation Configuration
simulation_config_id <- 1
sql_query <- paste("select * from simulation_config where id =", simulation_config_id)
simulation_config <- dbGetQuery(mydb, sql_query)

#-- Read Risk Configuration

#-- Read Simulation Calendar
game_calendar <- dbGetQuery(mydb, "select * from games_calendar where league_id = 1")

#-- Read Game Inventory
game_inventory <- dbGetQuery(mydb, "select * from bets")

#-- Begin Simulation
calendar_day <- start_day

#-- For Each Day in Simulation
number_of_simulation_days <- simulation_config$simulation_days
capital <- 1

for (day in 1:number_of_simulation_days) {
  
  nba_games  <- as.numeric(game_calendar %>% filter(league_id == 1, day_of_year == calendar_day) %>% select(number_of_games))
  
  if ( !(nba_games == 0) ) {
  
    nba_bet_id <- game_inventory %>% filter(league_id == 1) %>% select(id)
    nba_bet_id <- sample(nba_bet_id$id, 2*nba_games, replace=FALSE)
    
    nba_games <- game_inventory[nba_bet_id,]
    nba_games$kelly_bet <- get_kelly_bet(nba_games)
    
    nba_games <- nba_games %>% filter(kelly_bet > 0)
    nba_games <- nba_games %>% mutate(wagered = kelly_bet / sum(kelly_bet) / 4) %>% arrange(-kelly_bet)
    nba_games$odds <- convert_moneyline_to_odds(nba_games)
    
    win_loss <- get_simulation_outcomes(1, 0.025, nba_games)
    
    profit <- get_profit_loss(win_loss, nba_games)
    
    capital <- capital* (1 + profit)
    
    print(paste(calendar_day, round(profit, 3), round(capital,3)))
    
  }
  
  calendar_day <- ifelse(calendar_day >= 364, 1, calendar_day + 1)
  
}

  #-- Create Bets for Simulation Day
  
  #-- Identify Wagers for Simulation Day
  
  #-- Simulate Results
  
  #-- Apply Business Rules from Risk Configuration
  
  #-- Store Data

#-- Disconnect from Database
dbDisconnect(mydb)