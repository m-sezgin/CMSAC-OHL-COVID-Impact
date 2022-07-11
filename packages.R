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
try <- nhl_players_allseasons(playerIds = c(91435, 90194))


# fastRhockey -------------------------------------------------------------

install.packages("fastRhockey")
library(fastRhockey)
try3 <- fastRhockey::load_nhl_player_box(2021)
jack <- try3 %>% filter(player_full_name == "Jack Hughes")

nhl_player_stats(player_id = 8481559)
jack2 <- nhl_player_info(8481559)
