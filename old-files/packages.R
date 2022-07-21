# load libraries
library(tidyverse)


# elite package -----------------------------------------------------------

#install.packages("devtools")
library(devtools)
#devtools::install_github("eoppe1022/elite", force = TRUE)

library(elite)

draft21 <- get_drafts("nhl entry draft", 2021)
stats <- get_player_stats_individual(draft21)

try <- draft21 %>% 
  filter(draft_team == "New Jersey Devils") %>% 
  get_player_stats_individual()

try2 <- draft21 %>% 
  filter(name == "Luke Hughes") %>% 
  get_player_stats_individual()

# nhl api -----------------------------------------------------------------

#install.packages("nhlapi")
library(nhlapi)

draft <- nhl_draft_prospects() # gives ranks
# nhl_players_allseasons(playerIds = c(91435, 90194))

try <- nhl_teams_stats(1:2, c("20052006", "20062007", "20072008"))

try2 <- nhl_get_data("https://statsapi.web.nhl.com/api/v1/teams/1",
  flatten = FALSE)
try3 <-  nhl_get_data(c(
  "https://statsapi.web.nhl.com/api/v1/teams/1",
  "https://statsapi.web.nhl.com/api/v1/people/8477474",
  flatten = TRUE
))
try4 <-  nhl_players(playerIds = c(8451101, 8458554)) # doesn't give stats
try5 <- nhl_drafts(2015:2017)

# fastRhockey -------------------------------------------------------------

library(fastRhockey)
try3 <- fastRhockey::load_nhl_player_box(2021)
jack <- try3 %>% filter(player_full_name == "Jack Hughes")

nhl_player_stats(player_id = 8481559)
jack2 <- nhl_player_info(8481559)


# nhl scrape --------------------------------------------------------------

install.packages("nhlscrape")
library(nhlscrape)

# set location
SetDbPath("C:/Users/jacki/Desktop/REU/hockey/CMSAC-NHL-COVID-Impact")

# select the kraken
AddAllTeamsDb()
team_id <- GetTeamId("Seattle Kraken")
gids <- GetGameIdRange(team_id, "2019-09-30", "2019-12-18")
