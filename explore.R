# purpose: initial exploration of data


# load packages -----------------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(ggbeeswarm) 
library(ggcorrplot)
library(broom)
library(ggfortify)


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
length(unique(recent$team_name))

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

recent %>% 
  group_by(season, name) %>% 
  summarize(avg = mean(ppg))


recent %>% 
  ggplot(aes(x = g)) +
  geom_histogram() +
  facet_wrap(~season)


# draft -------------------------------------------------------------------
recent %>% 
  filter(!is.na(draft_year)) %>% 
  summarize(count = n())
  View()
# 398 players drafted -> 18.9%

# make dataframe of drafted players
drafted <- recent %>% 
  filter(!is.na(draft_year))

summary(drafted)
table(drafted$draft_year)
drafted %>% filter(draft_year == "2017") %>% View()

drafted %>% 
  ggplot(aes(x = log(pts))) +
  geom_histogram(aes(fill = factor(draft_year)), alpha = 0.5) +
  theme_bw()
drafted %>% 
  ggplot(aes(y = pts)) +
  geom_violin(aes(x = "")) +
  coord_flip()
drafted %>% 
  ggplot(aes(x = pts)) +
  stat_ecdf()


# create column that says yes or no if they were drafted
recent2 <- recent %>% 
  mutate(got_drafted = case_when(!is.na(draft_year) ~ 'Yes',
                                 TRUE ~ 'No'))

recent2 %>% # fix
  ggplot(aes(x = gp, y = g, color = got_drafted)) +
  geom_line()

recent2 %>% 
  ggplot(aes(x = pts)) +
  geom_histogram(aes(fill = got_drafted), alpha = 0.5) +
  theme_bw()


# boxplot alternatives ----------------------------------------------------

# violin
recent %>% 
  ggplot(aes(x = ppg, y = factor(season))) +
  geom_violin(fill = "cornflowerblue") +
  stat_summary(fun = "mean", size = .25) + # show mean
  theme_bw() +
  coord_flip()

# jitter plot / strip chart
recent %>% 
  ggplot(aes(x = factor(season), y = ppg)) +
  geom_jitter(alpha = 0.4, aes(color = position),
              position=position_jitter(0.2)) +
  theme_bw()
recent %>% 
  ggplot(aes(x = draft_year, y = ppg)) +
  geom_jitter()

# beeswarm
recent %>%
  ggplot(aes(y = ppg)) +
  geom_beeswarm(aes(x = ""), cex = 3) +
  theme_bw() +
  coord_flip()

# density plot with rugs
recent %>% 
  ggplot(aes(x = ppg)) +
  # geom_density(aes(fill = season), alpha = 0.3) +
  geom_density(aes(color = season), alpha = 0.3) +
  geom_rug(alpha = 0.5) +
  theme_bw() 

recent %>% 
  ggplot(aes(x = ppg, y = factor(season))) +
  geom_dotplot(alpha = 0.5) +
  theme_bw()


mosaicplot(table(recent$position, recent$shoots))

# bean plot
# install.packages("beanplot")
library(beanplot)
beanplot(ppg ~ round, data = drafted)

## 
recent %>% 
  group_by(season) %>% 
  ggplot(aes(x = factor(season), y = gp)) +
  geom_jitter()

View(table(recent$league))


# confounding variables ---------------------------------------------------

## player quality

# drafted
recent2 %>% 
  ggplot(aes(x = ppg)) +
  geom_density(aes(fill = got_drafted), alpha = 0.3) +
  geom_rug(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~season)
recent2 %>% 
  ggplot(aes(x = ppg)) +
  geom_histogram(aes(fill = got_drafted), alpha = 0.5) +
  theme_bw() +
  labs(x = "points per game", fill = "drafted") +
  facet_wrap(~season)
summary(lm(ppg ~ got_drafted, data = recent2))

## new - assumptions
model1 <- lm(ppg ~ got_drafted, data = recent2)
autoplot(model1)

# draft pick number
summary(lm(ppg ~ overall_pick_num, data = drafted))
drafted %>% 
  ggplot(aes(x = gp, y = overall_pick_num)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  labs(x = "games played", y = "draft pick number") +
  theme_bw()
drafted %>% 
  ggplot(aes(x = gp)) +
  geom_density(aes(fill = factor(round)), alpha = 0.5) +
  theme_bw()

drafted %>% 
  ggplot(aes(x = factor(round), y = gp)) +
  geom_jitter(alpha = 0.4, aes(color = position),
              position=position_jitter(0.2)) +
  labs(x = "round", y = "games played") +
  theme_bw()
# is this for the numbers of the season before they got drafted?
# should we look at their NHL numbers (if they have any)?

mosaicplot(table(drafted$round, drafted$season))

# position
drafted %>% 
  ggplot(aes(x = draft_year)) +
  geom_histogram(aes(fill = position), alpha = 0.5)
drafted %>% 
  ggplot(aes(x = factor(position), y = gp)) +
  geom_jitter(alpha = 0.4, aes(color = draft_year),
              position=position_jitter(0.2)) +
  labs(x = "position", y = "games played", color = "draft year") +
  theme_bw()
drafted %>% 
  ggplot(aes(x = a, y = factor(position))) +
  geom_violin(fill = "violet") +
  theme_bw() +
  coord_flip()


drafted %>% 
  ggplot(aes(x = factor(position), y = gp)) +
  geom_jitter(alpha = 0.4, aes(color = round),
              position=position_jitter(0.2)) +
  labs(x = "position", y = "games played", color = "round") +
  theme_bw()

  
defensemen <- drafted %>% 
  filter(position == "D")
forwards <- drafted %>% 
  filter(position == "F")
summary(defensemen)
summary(forwards)

drafted %>% # doesn't do what I want
  group_by(position) %>% 
  summarize(mean = mean(c(gp, g, a, pts, ppg)),
            min = min(c(gp, g, a, pts, ppg)),
            max = max(c(gp, g, a, pts, ppg))) %>% 
  View()

summary(lm(ppg ~ round, data = drafted))



# overall year stats ------------------------------------------------------

sumyear <- recent %>% 
  group_by(name, season) %>% 
  summarise_at(vars(gp:pm, ppg), "sum")

# players with stats from both 2019-2020 and 2021-2022
summed_both <- recent %>% 
  filter(season %in% c("2019-2020", "2021-2022"))
summed_both <- summed_both %>% 
  group_by(name, season) %>% 
  summarize_at(vars(gp:pm, ppg), "sum")
summed_both <- summed_both %>% 
  group_by(name) %>% 
  mutate(count_seasons = n()) %>% 
  filter(count_seasons == 2)

# density plot
summed_both %>%
  ggplot(aes(x = ppg)) +
  geom_density(aes(fill = season), alpha = 0.3) +
  geom_rug(alpha = 0.5) +
  theme_bw()





drafted %>% 
  ggplot(aes(x = factor(position), y = gp)) +
  geom_jitter(alpha = 0.4, aes(color = draft_year),
              position=position_jitter(0.2)) +
  labs(x = "position", y = "games played", color = "draft year") +
  theme_bw()

recent2 <- recent %>% 
  mutate(got_drafted = case_when(!is.na(draft_year) ~ 'Yes',
                                 TRUE ~ 'No'))

# something with pairs??

recent %>% 
  ggplot(aes(x = ppg)) +
  geom_density(aes(fill = got_drafted), alpha = 0.3) +
  geom_rug(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~season)

test <- recent %>% filter(season == "2020-2021")
length(unique(test$league))

table(test$league)
