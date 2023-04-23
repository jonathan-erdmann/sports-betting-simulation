library(httr)
library(jsonlite)

api_key <- "749f478f25f0f60f8ccb79948827ee1f"

#url <- paste0("https://api.the-odds-api.com/v4/sports/?apiKey=",api_key)
#test <- GET(url)
#info <- fromJSON(rawToChar(test$content))

#Get MLB Odds
  mlb_key <- "baseball_mlb"
  url <- paste0("https://api.the-odds-api.com/v4/sports/",mlb_key,"/odds/?apiKey=",api_key,"&regions=us&markets=h2h")
  
  mlb_data_raw <- GET(url)
  
  mlb_data_json <- fromJSON(rawToChar(mlb_data_raw$content))
  
  mlb_data_prep <- mlb_data_json
  mlb_data_prep$date <- strtrim(mlb_data_json$commence_time, 10)
  mlb_data_prep <- mlb_data_prep %>% filter(date == Sys.Date()) %>% select("date","home_team","away_team")
  
  mlb_home_odds_list <- rep(0,nrow(mlb_data_prep))
  mlb_away_odds_list <- rep(0,nrow(mlb_data_prep))
  
  for (ii in 1:nrow(mlb_data_prep)) {
    
    mlb_home_odds_list[ii] <- data.frame((data.frame(mlb_data_json$bookmakers[ii]) %>% filter(key=="draftkings"))[[4]][[1]][[3]])$price[1] - 1
    mlb_away_odds_list[ii] <- data.frame((data.frame(mlb_data_json$bookmakers[ii]) %>% filter(key=="draftkings"))[[4]][[1]][[3]])$price[2] - 1
    
  }
  
  mlb_data_prep$home_odds <- mlb_home_odds_list
  mlb_data_prep$away_odds <- mlb_away_odds_list
  
  date_today <- rep(mlb_data_prep$date[1], 2*nrow(mlb_data_prep))
  team_name <- date_today
  odds <- date_today
  
  for (ii in 1:nrow(mlb_data_prep)) {
    
      team_name[2*ii - 1] <- mlb_data_prep$home_team[ii]
      odds[2*ii - 1] <- mlb_data_prep$home_odds[ii]
      
      team_name[2*ii] <- mlb_data_prep$away_team[ii]
      odds[2*ii] <- mlb_data_prep$away_odds[ii]
      
  }
  
  mlb_odds <- data.frame(date_today,team_name,odds)

#Get NHL Odds
nhl_key <- "icehockey_nhl"
url <- paste0("https://api.the-odds-api.com/v4/sports/",nhl_key,"/odds/?apiKey=",api_key,"&regions=us&markets=h2h")

#Get NBA Odds
nba_key <- "basketball_nba"
url <- paste0("https://api.the-odds-api.com/v4/sports/",nba_key,"/odds/?apiKey=",api_key,"&regions=us&markets=h2h")

#Get NFL Odds
nfl_key <- "americanfootball_nfl_super_bowl_winner"
url <- paste0("https://api.the-odds-api.com/v4/sports/",nfl_key,"/odds/?apiKey=",api_key,"&regions=us&markets=h2h")