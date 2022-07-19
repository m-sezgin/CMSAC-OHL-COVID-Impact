# PURPOSE: Fitting models/performing inference on OHL COVID data



# Load libraries ----------------------------------------------------------

library(tidyverse)
library(infer)

# Fit models --------------------------------------------------------------

# Load data
ohl <- read_csv("sams_ohl_data_request.csv")

# COVID years
ohl_covid <- ohl %>%
  filter(season %in% c("2019-2020","2020-2021", "2021-2022"))

# Hypothesis Testing Treatment vs non-Treatment ---------------------------

# Split players up into treatment vs non-treatment

ohl_20_22 <- ohl %>%
  filter(season %in% c("2020-2021", "2021-2022")) %>%
  mutate(ppg = pts/gp)

# Filtering only for players who played more than 10 games
ohl_20_21 <- ohl %>%
  filter(season == "2020-2021", gp > 10)

length(unique(ohl_20_21$player_id)) == nrow(ohl_20_21)

# Filter so only league -- wait this doesn't matter right?
# ohl_20_21 <- ohl_20_21 %>%
#   arrange(player_id, -gp) %>%
#   filter(duplicated(player_id) == FALSE)

ohl_21_22 <- ohl %>%
  filter(season == "2021-2022") %>%
  mutate(ppg = pts/gp)

ohl_treatment <- ohl_21_22 %>%
  filter(league == "OHL") %>%
  mutate(treatment = ifelse(player_id %in% ohl_20_21$player_id, "Played", "Didn't play"))

# Distribution of ppg by treatment for 21-22 season
ggplot(ohl_treatment, aes(x = ppg)) +
  geom_density() +
  facet_wrap(~ treatment)

ohl_means <- ohl_treatment %>%
  group_by(treatment) %>%
  summarize(average = mean(ppg))

## Start hypothesis test

# Calculate diff in means for both groups
obs_diff <- ohl_treatment %>%
  specify(ppg ~ treatment) %>%
  calculate(stat = "diff in means", order = c("Played", "Didn't play"))

# Create null distribution for diff in means
null_dist <-  ohl_treatment%>%
  specify(ppg ~ treatment) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Played", "Didn't play"))

# 
names(null_dist)

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

null_dist <- null_dist %>%
  mutate(big_diff = ifelse(stat > obs_diff$stat, "Played", "Didn't play"))

table(null_dist$big_diff)

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

# ggplot(ohl_treatment, aes(x = ppg)) +
#    geom_bar(y = mean())

# Modeling with ppg response ---------------------------------------------------

# We need to check that each player played in the OHL in 2019-2020 and 2021-2022
ohl_filtered <- ohl %>%
  filter(season %in% c("2019-2020", "2021-2022"), league == "OHL") %>%
  group_by(player_id, season) %>%
  mutate(played_both_szn = ifelse(n() > 1, TRUE, FALSE)) %>%
  filter(played_both_szn == TRUE) %>%
  left_join(select(ohl_treatment, player_id, treatment), by = "player_id") %>%
  mutate(ppg = pts/gp) %>%
  filter(ppg != 0) %>%
  mutate(log_ppg = log(ppg))

nrow(ohl_filtered)

## Fit models

# vanilla MLR
mlr <- lm(ppg ~ treatment + season + position, data = ohl_filtered)
summary(mlr)
plot(mlr)

# ppg on log scale
log_mlr <- lm(log_ppg ~ treatment + season + position, data = ohl_filtered)
summary(log_mlr)
plot(log_mlr)

# GLM??
glm <- glm(ppg ~ treatment + season,
           data = ohl_filtered,
           family = "")

# GAM
# set.seed(2004)
# ohl_gam <- ohl_filtered %>%
#   mutate(is_train = sample(rep(0:1, length.out = nrow(ohl_filtered))))

# library(mgcv)
# init_logit_gam <- gam(ppg ~ s(treatment) + s(season),
#                       data = filter(ohl_gam, is_train == 1), 
#                       family = gaussian, method = "REML")

