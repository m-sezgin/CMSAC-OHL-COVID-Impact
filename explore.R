# purpose: initial exploration of data


# load packages -----------------------------------------------------------

library(tidyverse)
library(RColorBrewer)


# load data ---------------------------------------------------------------

library(readr)
sams_ohl_data_request <- read_csv("C:/Users/jacki/Desktop/REU/hockey/sams_ohl_data_request.csv")
View(sams_ohl_data_request)


# explore data ------------------------------------------------------------

table(sams_ohl_data_request$season)

# filter years
recent <- sams_ohl_data_request %>% 
  filter(season %in% c("2019-2020", "2020-2021", "2021-2022"))
# note that 2019-2020 is 1145 and 2020-2021 is only 233

# add points per game columns
recent <- recent %>% 
  mutate(ppg = g/gp)

recent %>% 
  group_by(name) %>% 
  summarize(mean_goals = mean(ppg))
# A.J. Cook played very little games in 2020-2021 compared to other seasons

# games played vs goals scatterplot (first 100)
recent %>% 
  head(100) %>% 
  ggplot(aes(x = gp, y = g)) +
  geom_point(aes(color = name), alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "none")

recent %>% 
  head(20) %>% 
  group_by(name, season) # add

# points per game for 2020-2021 season
recent %>% 
  filter(season == "2020-2021") %>% 
  arrange(desc(ppg)) %>% 
  View()

# boxplot - position
recent %>% 
  ggplot(aes(x = ppg, y = position)) +
  geom_boxplot(aes(fill = position)) +
  labs(x = "Points Per Game", y = "Position") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")

# boxplot - season
recent %>% 
  ggplot(aes(x = ppg, y = season)) +
  geom_boxplot(aes(fill = season)) +
  labs(x = "Points Per Game", y = "Season") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues")

# boxplot - season + position
recent %>% 
  ggplot(aes(x = ppg, y = season)) +
  geom_boxplot(aes(fill = position)) +
  labs(x = "Points Per Game") +
  theme_bw() +
  scale_fill_brewer(palette = "Blues")

# mean points per game by season
recent %>% 
  filter(!is.na(ppg)) %>% 
  group_by(season) %>% 
  summarize(mean_goals = mean(ppg))

# how many leagues
length(unique(recent$league))

# how many players
length(unique(recent$name))

# separation by league ----------------------------------------------------

try <- recent %>% 
  filter(league != "OHL")
table(try$league, try$season)


# goals per game for all leagues by season
recent %>% 
  #group_by(league) %>% 
  filter(league != "OHL") %>% 
  ggplot(aes(x = gp, y = g, color = league)) +
  geom_point(alpha = 0.4) +
  labs(x = "Games Played", y = "Goals") +
  facet_wrap(~season) +
  theme_bw() +
  theme(legend.position = "none")

# regression for one league
recent %>% 
  filter(league == "International-Jr") %>% 
  ggplot(aes(x = gp, y = g)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(x = "Games Played", y = "Goals") +
  facet_wrap(~season) +
  theme_bw()


# fit linear regression
init_lm <- lm(ppg ~ league + position + season, data = recent)
summary(init_lm)

summary(lm(ppg ~ factor(league) + factor(season) + factor(position), 
           data = all_seasons)) # recent or all seasons?

plot(x = init_lm) # linear regression plot

recent %>% # gives error
  mutate(pred_vals = predict(init_lm)) %>%
  ggplot(aes(x = pred_vals,
             y = ppg)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "red",
              size = 2) +
  theme_bw()

# glm
# help
init_logit <- glm(ppg ~ league + position + season,
                  data = recent,
                  family = "poisson")

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values) %>% # edit to this data
  ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred_prob), 
            color = "blue") +
  geom_point(aes(y = is_fg_made), 
             alpha = 0.3,
             color = "darkorange") +
  theme_bw()


init_logit2 <- glm(ppg ~ league + position + season,
                  data = recent,
                  family = "gaussian")
summary(init_logit2)

table(recent$league, recent$season)

# leagues with data for all seasons
all3 <- recent %>% 
  filter(season %in% c("2019-2020", "2020-2021", "2021-2022")) %>%
  group_by(league, season) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = season, values_from = count) %>%
  filter(!is.na(`2019-2020`) & !is.na(`2020-2021`) & !is.na(`2021-2022`))

all_seasons <- recent %>% 
  filter(league %in% c("Belarus Cup", "International-Jr", "MHL",
                       "NOJHL", "Slovakia U20", "Slovakia2",
                       "USHL"))
all_seasons %>% 
  group_by(league) %>% 
  ggplot(aes(x = gp, y = g)) +
  geom_point(aes(color = league)) +
  facet_wrap(~season) +
  geom_smooth() +
  theme_bw()

# players in all three seasons
all_seasons %>% 
  group_by(name) %>% 
  summarize(count = n())

##
# look out how players numbers changed throughout the seasons - work on
all_seasons %>% 
  group_by(name, season) %>% 
  summarize(points = mean(ppg)) # add

##


