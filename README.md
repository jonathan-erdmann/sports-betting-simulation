## Sports Betting Simulation Description
Risk aversion adjusted optimized capital allocation for sports bets
  Author: Jonathan Erdmann

Author: Jonathan Erdmann

## Project Goals
  1. Determine feasibility of betting strategy with simulated outcomes from real data
  2. Establish semi-optimized business rules for long-term profitability
  3. Automate capital allocation and resourcing for long-term profitability

## Project Details
  The project will be written in R and utilize the RSQLite package for persistent data

## Package Dependencies
  - DBI
  - RSQLite
  - tidyverse
  - dplyr
  - tibble
  - sqldf
  - ggplot2
  - reader
  
  
## Function List

  As of 2023-04-23

# Prototyping
        get_mlb_odds
        get_nba_odds
        get_nhl_odds

# Scrape Betting Data
        get_nhl_win_probabilities
        get_mlb_win_probabilities
        get_nba_win_probabilities
        get_nba_money_lines
        get_mlb_money_lines
        get_nhl_money_lines
        get_mlb_scores
        get_nba_scores
        get_nhl_scores

# Utilities
        get_kelly_bet
        convert_moneyline_to_probability
        get_simulation_outcomes
        convert_moneyline_to_odds
        get_simulation_profit_loss
        attach_game_id
        get_nba_daily_bets
        get_mlb_daily_bets
        get_nhl_daily_bets
        store_nba_games_to_db
        store_mlb_games_to_db
        store_nhl_games_to_db
        get_simulated_bets
        get_kelly_bet_from_distribution
        get_bet_returns
        get_profit_loss
        get_return_on_wager
        get_estimated_return_percentile