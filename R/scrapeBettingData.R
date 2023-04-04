library(rvest)
library(dplyr)

#-- Return NHL Win Probabilities
get_nhl_win_probabilities <- function() {
 
  #-- Source URL
  url <- "https://projects.fivethirtyeight.com/2023-nhl-predictions/games/"
  
  #-- Note that format of NHL predictions differs, includes repetitions and does not have an easy to fetch date tag for each game
  #-- Data frame requires post-function filtering
  
  webpage <- read_html(url)
  
  #-- Get Team Names
  team_name <- webpage %>% html_nodes(".team-name") %>% html_text()
  
  #-- Get Win Probabilities
  win_probability <- webpage %>% html_nodes(".num.win-prob") %>% html_text()
  win_probability <- as.numeric(sub("%","",win_probability)) / 100
  
  #-- League ID
  league_id <- 4
  league <- rep(league_id, length(win_probability))
  
  #-- Return Probabilities after removing duplicates
  nhl_win_probabilities <- data.frame(league, team_name, win_probability) %>% distinct(league, team_name, win_probability, .keep_all = TRUE)
  
  return(nhl_win_probabilities)
  
}

#-- Return MLB Win Probabilities
get_mlb_win_probabilities <- function() {
  
  #-- Source URL
  url <- "https://projects.fivethirtyeight.com/2023-mlb-predictions/games/"
  
  webpage <- read_html(url)
  
  #-- Today's Date in 538 Format for filtering larger games table
  short_date <- paste0(as.numeric(format(Sys.Date(),"%m")),"/",as.numeric(format(Sys.Date(),"%d")))
  
  #-- Dates of games within upcoming games title
  date <- webpage %>% html_element("tbody") %>% html_elements(".day.short") %>% html_text()
  
  #-- Number of teams playing on today's date is 2 x number of games occurring today
  number_of_teams <- 2 * sum(short_date == date)
  
  #-- Get Team Names
  team_name <- webpage %>% html_element("tbody") %>% html_elements(".team-name.long") %>% html_text()
  team_name <- team_name[1:number_of_teams]
  
  #-- Get Win Probabilities
  win_probability <- webpage %>% html_element("tbody") %>% html_elements(".win-prob") %>% html_text()
  win_probability <- win_probability[1:number_of_teams]
  win_probability <- as.numeric(sub("%","",win_probability)) / 100
  
  #-- League ID
  league_id <- 2
  league <- rep(league_id, length(win_probability))
  
  #-- Return Probabilities
  mlb_win_probabilities <- data.frame(league, team_name, win_probability)
  
  return(mlb_win_probabilities)  
  
}

#-- Return NBA Win Probabilities
get_nba_win_probabilities <- function() {
  
  #-- Source URL
  url <- "https://projects.fivethirtyeight.com/2023-nba-predictions/games/"
  
  webpage <- read_html(url)
  
  #-- Include only today's data
  today <- webpage %>% html_nodes(".day")
  today <- today[1]
  
  #-- Fetch Win Percentages
  win_probability <- today %>% html_nodes("tr.team") %>% html_elements(".chance") %>% html_text()
  win_probability <- as.numeric(sub("%","",win_probability)) / 100
  
  #-- Fetch Team Names
  team_name <- today %>% html_nodes("tr.team") %>% html_elements(".team") %>% html_text()
  
  #-- League ID
  league_id <- 1
  league <- rep(league_id, length(win_probability))
  
  #-- Return Probabilities
  nba_win_probabilities <- data.frame(league, team_name, win_probability)
  
  return(nba_win_probabilities)
  
}

get_nba_money_lines <- function() {
  
  url <- "https://sportsbook.draftkings.com/leagues/basketball/nba"
  #https://sportsbook.draftkings.com/leagues/baseball/mlb
  #https://sportsbook.draftkings.com/leagues/hockey/nhl
  #https://sportsbook.draftkings.com/leagues/football/nfl
  
  webpage <- read_html(url)
  
  #-- Fetch Team Names and remove city code
  team_name <- webpage %>% html_nodes(".event-cell__name-text") %>% html_text()
  team_name <- sub(".*? ","", team_name)
  
  #-- Fetch Money Lines
  moneyline <- as.numeric(sub("−","-",webpage %>% html_nodes(".american.no-margin") %>% html_text()))
  
  #-- League ID
  league_id <- 1
  league <- rep(league_id, length(moneyline))
  
  #-- Return Probabilities
  money_lines <- data.frame(league, team_name, moneyline)
  
  return(money_lines)
  
}

get_mlb_money_lines <- function() {
  
  url <- "https://sportsbook.draftkings.com/leagues/baseball/mlb"
  #"https://sportsbook.draftkings.com/leagues/basketball/nba"
  #https://sportsbook.draftkings.com/leagues/hockey/nhl
  #https://sportsbook.draftkings.com/leagues/football/nfl
  
  webpage <- read_html(url)
  
  #-- Fetch Team Names and remove city code
  team_name <- webpage %>% html_nodes(".event-cell__name-text") %>% html_text()
  team_name <- sub(".*? ","", team_name)
  
  #-- Fetch Money Lines
  moneyline <- as.numeric(sub("−","-",webpage %>% html_nodes(".american.no-margin") %>% html_text()))
  
  #-- League ID
  league_id <- 2
  league <- rep(league_id, length(moneyline))
  
  #-- Return Probabilities
  money_lines <- data.frame(league, team_name, moneyline)
  
  return(money_lines)
  
}

get_mlb_scores <- function() {
  
  date_string <- gsub("-","",Sys.Date() - 1)
  url <- paste0("https://www.espn.com/mlb/scoreboard/_/date/", date_string)
  
  webpage <- read_html(url)
  
  team_name <- webpage %>% html_elements("div") %>% html_elements(".truncate.db") %>% html_text()

  if (length(team_name) == 0) {
    
    return(0)
    
  } else {
  
    score <- webpage %>% html_elements("div") %>% html_elements(".pl2.baseball") %>% html_text()
    score <- matrix(as.numeric(score), nrow = 3)[1,]
    odd_wins  <- ifelse(score[c(TRUE,FALSE)] > score[c(FALSE,TRUE)], 1, 0)
    even_wins <- 1 - odd_wins
    
    win <- rep(0, length(score))
    win[c(TRUE,FALSE)] <- odd_wins
    win[c(FALSE,TRUE)] <- even_wins
    
    win <- data.frame(team_name, win)
    
    return(win)

  }
  
}

get_nba_scores <- function(iOffset = 1) {
  
  date_string <- gsub("-","",Sys.Date() - iOffset)
  url <- paste0("https://www.espn.com/nba/scoreboard/_/date/", date_string)
  
  webpage <- read_html(url)
  
  team_name <- webpage %>% html_elements("div") %>% html_elements(".truncate.db") %>% html_text()
  
  if (length(team_name) == 0) {
    
    return(0)
    
  } else {
  
    score <- as.numeric(webpage %>% html_elements("div") %>% html_elements(".fw-heavy") %>% html_text())
    odd_wins  <- ifelse(score[c(TRUE,FALSE)] > score[c(FALSE,TRUE)], 1, 0)
    even_wins <- 1 - odd_wins
    
    win <- rep(0, length(score))
    win[c(TRUE,FALSE)] <- odd_wins
    win[c(FALSE,TRUE)] <- even_wins
    
    win <- data.frame(team_name, win)
    
    return(win)
    
  }
  
}