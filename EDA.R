# PURPOSE: EDA on OHL COVID impact

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)

# Explore data ------------------------------------------------------------

ohl <- read_csv("sams_ohl_data_request.csv")
ohl_covid <- ohl %>%
  filter(season %in% c("2019-2020","2020-2021", "2021-2022"))

## Add treatment var
# Split players up into treatment vs non-treatment

ohl_20_22 <- ohl %>%
  filter(season %in% c("2020-2021", "2021-2022")) %>%
  mutate(ppg = pts/gp)

# Filtering only for players who played more than 10 games
ohl_20_21 <- ohl %>%
  filter(season == "2020-2021", gp > 10)

length(unique(ohl_20_21$player_id)) == nrow(ohl_20_21)

ohl_21_22 <- ohl %>%
  filter(season == "2021-2022") %>%
  mutate(ppg = pts/gp)

ohl_treatment <- ohl_21_22 %>%
  filter(league == "OHL") %>%
  mutate(treatment = ifelse(player_id %in% ohl_20_21$player_id, "Played", "Didn't play"))

# Distribution of ppg by treatment for 21-22 season
ggplot(ohl_treatment, aes(x = ppg, color = treatment)) +
  geom_density()

# How many observations per season?
table(ohl_covid$season)

# Distribution of games played during 2019-22?

ggplot(ohl_covid, aes(x = gp)) +
  geom_density() +
  facet_grid(rows = vars(season)) +
  theme_bw()

# in 2021 a lot of people didn't play a lot, which makes sense.
# In 2019-20 and 2021-22, a good amount of players didn't play a lot but
# there's a dip at the 20 game mark, after which number of players starts to
# pick up

# Create player "ratings" (points per game)
ohl_ratings <- ohl_covid %>%
  mutate(ppg = pts/gp)

# Density curve of ppg for all players in all leagues on all teams by position
ggplot(ohl_ratings, aes(x = ppg)) +
  geom_density() +
  facet_wrap(~ position)

# adding age to dataset
ohl_ratings <- ohl_ratings %>%
  filter(season %in% c("2019-2020", "2020-2021", "2021-2022")) %>%
  mutate(age = trunc((dob %--% as.Date("2022-01-01")) / years(1))) %>%
  filter(age %in% 16:22, league == "OHL")

# Density curve of ppg faceted by age
ggplot(ohl_ratings, aes(x = ppg)) +
  geom_density() +
  # facet_wrap(~ age, nrow = 7)
  facet_grid(rows = vars(age))

# what's the difference between nrow of all ohl players vs ohl players
# with at least 10 games played?
nrow(ohl_ratings)
nrow(filter(ohl_ratings, gp >= 10))

# -> not that different... only a reduction of 80 players

# plot of ppg vs age with linear model
ohl_ratings %>%
  filter(gp >= 10) %>%
  ggplot(aes(x = age, y = ppg)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm")

# colored by position
ohl_ratings %>%
  filter(gp >= 10) %>%
  ggplot(aes(x = age, y = ppg, color = position)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm")

# Age significantly positively correlated with ppg
summary(lm(ppg ~ age, data = ohl_ratings))

# Look at distribution/relationship for player quality once I figure out how to
# compute it



# For Jackie: filtering for leagues that are present in all seasons 2019-2022 ---------

ohl_covid %>%
  group_by(league, season) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = season, values_from = count) %>%
  filter(!is.na(`2019-2020`) & !is.na(`2020-2021`) & !is.na(`2021-2022`))
