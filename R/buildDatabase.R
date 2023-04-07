#-- Create Betting Simulation Database
library(DBI)
library(RSQLite)
library(data.table)

# Running from sports-betting-simulation/R
# Database location: sports-betting-simulation/db
# Data location: sports-betting-simulation/data

dbFileName <- "bettingSimulation.sqlite"
dbPath <- file.path("C:","Users","Jonathan","Documents","Jonathan","r-projects","sports-betting-simulation", "db")
dbFile <- paste0(dbPath,"/" ,dbFileName)

#-- Actions
createSchema <- 0
populateData <- 1

#Connect to database
mydb <- dbConnect(RSQLite::SQLite(), dbFile)

#-- Create schema
if (createSchema) {
  
  # Create Bets table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS bets
    (
       id integer primary key
      ,game_id text
      ,league_id integer
      ,date date
      ,team_id integer
      ,location text
      ,team_name text
      ,win_probability real
      ,moneyline integer
      ,odds real
      ,kelly_bet real
    )
  ")
  
  # Create Leagues table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS leagues
    (
       id integer primary key
      ,name text
      ,sport text
      ,league text
    )
  ")
  
  # Create Teams table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS teams
    (
       id integer primary key
      ,league_id integer
      ,location text
      ,team_name text
    )
  ")
  
  # Create Outcomes table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS outcomes
    (
       id integer primary key
      ,bets_id integer
      ,team_id integer
      ,win integer
      ,margin integer
    )
  ")
  
  # Create Risk Configuration table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS risk_config
    (
       id integer primary key
      ,total_bet_max real
      ,ind_bet_min real
      ,ind_bet_max real
      ,n_bets_min integer
      ,starting_working_capital real
      ,starting_reserve_capital real
      ,min_working_to_replenish real
      ,min_working_to_bank_no_replenish real
      ,min_working_to_bank_with_replenish real
      ,min_total_capital_to_rebalance real
    )
  ")
  
  # Create Simulation Config table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS simulation_config
    (
       id integer primary key
      ,risk_configuration integer
      ,allocation_id integer
      ,playable_leagues text
      ,simulation_days integer
      ,n_simulation_runs integer
      ,random_seed integer
    )
  ")
  
  # Create Simulated Potential Bets table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS simulation_potential_bets
    (
       simulation_id integer
      ,simulation_bet_id integer
      ,betting_day integer
      ,team_id integer
      ,win_probability real
      ,money_line real
    )
  ")
  
  # Create Simulated Wagered Bets table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS simulation_wagered_bets
    (
       simulation_id integer
      ,simulation_bet_id integer
      ,betting_day integer
      ,percentage_of_capital real
    )
  ")
  
  # Create Simulated Capital Values table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS simulation_capital_values
    (
       simulation_id integer
      ,simulation_bet_id integer
      ,run integer
      ,wagered_capital real
      ,returned_capital real
      ,profit_loss real
    )
  ")
  
  # Create Simulation Summaries table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS simulation_summary
    (
       simulation_id integer
      ,number_of_runs integer
      ,probability_of_win real
      ,estimated_geometric_return real
      ,estimated_geometric_return_interval_25 real
      ,estimated_geometric_return_interval_45 real
      ,estimated_geometric_return_interval_475 real
    )
  ")
  
  # Create Capital Allocation Method table
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS capital_allocation_method
    (
       id integer
      ,method text
    )
  ")
  
  
  # Create Games Calendar for Simulation
  dbExecute(mydb, "
    CREATE TABLE IF NOT EXISTS games_calendar
    (
       league_id integer
      ,day_of_year integer
      ,number_of_games integer
    )
  ")
  
}

if (populateData) {
  
  #-- Data Location
  filePath <- file.path("C:","Users","Jonathan","Documents","Jonathan","r-projects","sports-betting-simulation", "data")
  
  #-- Load Leagues
  fileName <- "leagues.csv"
  csvFile  <- paste0(filePath,"/" ,fileName)
  xfer_df  <- fread(csvFile)
  #dbWriteTable(mydb, "leagues", xfer_df, overwrite = FALSE, append = TRUE)
  
  #-- Load Teams
  fileName <- "teams.csv"
  csvFile  <- paste0(filePath,"/" ,fileName)
  xfer_df  <- fread(csvFile)
  #dbWriteTable(mydb, "teams", xfer_df, overwrite = FALSE, append = TRUE)
  
  #-- Load Game Calendar
  fileName <- "game_calendar.csv"
  csvFile  <- paste0(filePath,"/" ,fileName)
  xfer_df  <- fread(csvFile)
  #dbWriteTable(mydb, "games_calendar", xfer_df, overwrite = FALSE, append = TRUE)
  
}

dbDisconnect(mydb)