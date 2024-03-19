################################################################################
##### Load libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(janitor)

library(hablar)
library(ggplot2)
library(gt)
library(jsonlite)
library(httr)
library(nbastatR)
library(hoopR)
library(ggrepel)
library(ggthemes)
library(gtExtras)
library(ggtext)

################################################################################
##### Load data
##### Only considering players with >= 10 games played and >= 5 mpg
### Play types
# Set headers
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)
# Function to return stats from play_type page
get_shot_type_data <- function(play_type) {
  
  url <- paste0("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=", play_type, "&PlayerOrTeam=P&SeasonType=Regular%20Season&SeasonYear=2023-24&TypeGrouping=offensive")
  
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  ## Clean data
  df <- df %>%
    retype() %>%
    clean_names()  %>%
    as_tibble()
  # Remove "Jr."; "Sr." "III", etc...
  df <- df %>%
    mutate(player_name = gsub(" Jr.", "", player_name),
           player_name = gsub(" Sr.", "", player_name),
           player_name = gsub(" III", "", player_name),
           player_name = gsub(" II", "", player_name),
           player_name = gsub(" IV", "", player_name))
  # F. LastName -- better for plotting
  df$short_player_name <- paste0(substr(df$player_name, 1, 1), ". ", word(df$player_name, -1))
  return(df)
  
}
# load("off_data.rda")
# All play type pages
play_type_pages <- c("Isolation",
                     "Transition",
                     "PRBallHandler",
                     "PRRollMan",
                     "Postup",
                     "Spotup",
                     "Handoff",
                     "Cut",
                     "OffScreen",
                     "OffRebound")
# Map to one data frame
play_type_data <- map_df(play_type_pages, get_shot_type_data)
# Load in positional and minutes per game data
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
player_positions <- bref_players_stats(seasons = 2024,
                                       tables = "totals") %>%
  mutate(mpg = minutesTotals / countGames,
         # extract most recent team (used for shot chart)
         team_abbr = str_sub(slugTeamsBREF, -3, -1),
         ppg = round(ptsTotals / countGames, 1),
         rpg = round(trbTotals / countGames, 1),
         apg = round(astTotals / countGames, 1)
  ) %>%
  select(idPlayerNBA, groupPosition, countGames, mpg, ppg, pctFG, pctFG3, rpg, apg,
         team_abbr) %>%
  mutate(
    groupPosition = case_when(
      groupPosition == "G" ~ "Guard",
      groupPosition == "F" ~ "Forward",
      groupPosition == "C" ~ "Center"
    ),
    team = case_when(
      team_abbr == "MIA" ~ "Miami Heat",
      team_abbr == "MEM" ~ "Memphis Grizzlies",
      team_abbr == "MIN" ~ "Minnesota Timberwolves",
      team_abbr == "PHO" ~ "Phoenix Suns",
      team_abbr == "CLE" ~ "Cleveland Cavaliers",
      team_abbr == "NOP" ~ "New Orleans Pelicans",
      team_abbr == "MIL" ~ "Milwaukee Bucks",
      team_abbr == "ORL" ~ "Orlando Magic",
      team_abbr == "NYK" ~ "New York Knicks",
      team_abbr == "WAS" ~ "Washington Wizards",
      team_abbr == "POR" ~ "Portland Trail Blazers",
      team_abbr == "CHO" ~ "Charlotte Hornets",
      team_abbr == "PHI" ~ "Philadelphia 76ers",
      team_abbr == "SAS" ~ "San Antonio Spurs",
      team_abbr == "SAC" ~ "Sacramento Kings",
      team_abbr == "TOR" ~ "Toronto Raptors",
      team_abbr == "ATL" ~ "Atlanta Hawks",
      team_abbr == "CHI" ~ "Chicago Bulls",
      team_abbr == "LAC" ~ "Los Angeles Clippers",
      team_abbr == "DEN" ~ "Denver Nuggets",
      team_abbr == "BRK" ~ "Brooklyn Nets",
      team_abbr == "BOS" ~ "Boston Celtics",
      team_abbr == "HOU" ~ "Houston Rockets",
      team_abbr == "DAL" ~ "Dallas Mavericks",
      team_abbr == "IND" ~ "Indiana Pacers",
      team_abbr == "LAL" ~ "Los Angeles Lakers",
      team_abbr == "DET" ~ "Detroit Pistons",
      team_abbr == "UTA" ~ "Utah Jazz",
      team_abbr == "GSW" ~ "Golden State Warriors",
      team_abbr == "OKC" ~ "Oklahoma City Thunder"
    )
  ) %>% 
  # filter to at least 10 games and 5 mpg
  filter(countGames >= 10, mpg >= 5)
# add position column to analysis
play_type_data <- play_type_data %>%
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>%
  mutate(
    fga_thirty_six = (fga / mpg) * 36
  )

### Advanced Stats Table
# real url: https://www.nba.com/stats/players/advanced
url_advanced <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
res <- GET(url = url_advanced, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
advanced <- data.frame(json_resp$resultSets$rowSet)
colnames(advanced) <- json_resp[["resultSets"]][["headers"]][[1]]
## Clean data
advanced <- advanced %>%
  retype() %>%
  clean_names()  %>%
  as_tibble() %>%
  # Remove "Jr."; "Sr." "III", etc...
  mutate(player_name = gsub(" Jr.", "", player_name),
         player_name = gsub(" Sr.", "", player_name),
         player_name = gsub(" III", "", player_name),
         player_name = gsub(" II", "", player_name),
         player_name = gsub(" IV", "", player_name)) %>%
  # Add position and minutes per game
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA"))
advanced$short_player_name <- paste0(substr(advanced$player_name, 1, 1), ". ", word(advanced$player_name, -1))
### Closest Defenders Table
# Real url: https://www.nba.com/stats/players/shots-closest-defender
get_closest_defender_data <- function(spacecode, space) {
  url_distance <- paste0("https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=", spacecode, "&College=&Conference=&Country=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight=")
  res <- GET(url = url_distance, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  closest_defender <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(closest_defender) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  closest_defender$space <- space
  
  return(closest_defender)
}
spacecodes <- rep(c("0-2+Feet+-+Very+Tight", "2-4+Feet+-+Tight",
                    "4-6+Feet+-+Open", "6%2B+Feet+-+Wide+Open"), 1)
spaces <- rep(c("Very tight", "Tight", "Open", "Wide open"), 1)
#call function
closest_defender <- map2_df(spacecodes, spaces, get_closest_defender_data)

# Clean data
closest_defender <- closest_defender %>%
  retype() %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(player_name = gsub(" Jr.", "", player_name),
         player_name = gsub(" Sr.", "", player_name),
         player_name = gsub(" III", "", player_name),
         player_name = gsub(" II", "", player_name),
         player_name = gsub(" IV", "", player_name)) %>%
  # Add position and minutes per game
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA"))
### Defense Table
# real url <- https://www.nba.com/stats/players/defense
url_defense <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Defense&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
res <- GET(url = url_defense, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
defense <- data.frame(json_resp$resultSets$rowSet)
colnames(defense) <- json_resp[["resultSets"]][["headers"]][[1]]
## Clean data
defense <- defense %>%
  retype() %>%
  clean_names()  %>%
  as_tibble() %>%
  # Remove "Jr."; "Sr." "III", etc...
  mutate(player_name = gsub(" Jr.", "", player_name),
         player_name = gsub(" Sr.", "", player_name),
         player_name = gsub(" III", "", player_name),
         player_name = gsub(" II", "", player_name),
         player_name = gsub(" IV", "", player_name)) %>%
  # Add position and minutes per game
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA"))
### Hustle Table
# real url <- https://www.nba.com/stats/players/hustle
url_hustle <- "https://stats.nba.com/stats/leaguehustlestatsplayer?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&TeamID=0&VsConference=&VsDivision=&Weight="
res <- GET(url = url_hustle, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
hustle <- data.frame(json_resp$resultSets$rowSet)
colnames(hustle) <- json_resp[["resultSets"]][["headers"]][[1]]
## Clean data
hustle <- hustle %>%
  retype() %>%
  clean_names()  %>%
  as_tibble() %>%
  # Remove "Jr."; "Sr." "III", etc...
  mutate(player_name = gsub(" Jr.", "", player_name),
         player_name = gsub(" Sr.", "", player_name),
         player_name = gsub(" III", "", player_name),
         player_name = gsub(" II", "", player_name),
         player_name = gsub(" IV", "", player_name)) %>%
  # Add position and minutes per game
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>% 
  select(
    player_id,
    player_name,
    groupPosition,
    countGames,
    mpg,
    deflections,
    loose_balls_recovered,
    charges_drawn,
    contested_shots
  )
### Boxout Table
# real url <- https://www.nba.com/stats/players/box-outs
url_boxout <- "https://stats.nba.com/stats/leaguehustlestatsplayer?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&TeamID=0&VsConference=&VsDivision=&Weight="
res <- GET(url = url_boxout, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
boxout <- data.frame(json_resp$resultSets$rowSet)
colnames(boxout) <- json_resp[["resultSets"]][["headers"]][[1]]
## Clean data
boxout <- boxout %>%
  retype() %>%
  clean_names()  %>%
  as_tibble() %>%
  # Remove "Jr."; "Sr." "III", etc...
  mutate(player_name = gsub(" Jr.", "", player_name),
         player_name = gsub(" Sr.", "", player_name),
         player_name = gsub(" III", "", player_name),
         player_name = gsub(" II", "", player_name),
         player_name = gsub(" IV", "", player_name)) %>%
  # Add position and minutes per game
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>% 
  select(
    player_id,
    off_boxouts,
    def_boxouts
  )
# Shot chart data
nba_teams <- c("Dallas Mavericks", "Atlanta Hawks", "Denver Nuggets",
               "Houston Rockets", "Indiana Pacers", "Oklahoma City Thunder",
               "Boston Celtics", "New York Knicks", "Chicago Bulls",
               "Sacramento Kings", "Los Angeles Clippers", "Milwaukee Bucks",
               "Golden State Warriors", "Portland Trail Blazers", "Orlando Magic",
               "Los Angeles Lakers", "Minnesota Timberwolves", "Washington Wizards",
               "Brooklyn Nets", "Detroit Pistons", "Miami Heat",
               "San Antonio Spurs", "Phoenix Suns", "New Orleans Pelicans",
               "Charlotte Hornets", "Utah Jazz", "Toronto Raptors",
               "Philadelphia 76ers", "Cleveland Cavaliers", "Memphis Grizzlies")
team_shot_data <- teams_shots(teams = nba_teams, seasons = 2024)
team_shot_data <- team_shot_data %>%
  mutate(namePlayer = gsub(" Jr.", "", namePlayer),
         namePlayer = gsub(" Sr.", "", namePlayer),
         namePlayer = gsub(" III", "", namePlayer),
         namePlayer = gsub(" II", "", namePlayer),
         namePlayer = gsub(" IV", "", namePlayer))


save(boxout, defense, play_type_data, player_positions,
     advanced, closest_defender, hustle, team_shot_data, file = "nbadata.rda")
