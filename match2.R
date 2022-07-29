# second attempt at matching


# load packages -----------------------------------------------------------

library(tidyverse)
library(MatchIt)
library(mgcv)
# install.packages("cobalt")
library(cobalt)
# install.packages("lmtest") #coeftest
library(lmtest)
# install.packages("sandwich") #vcovCL
library(sandwich)
# install.packages("optmatch")
library(optmatch)


# load data ---------------------------------------------------------------

ohl_filtered <- read_csv("ohl_filtered.csv")

ohl_filtered <- ohl_filtered %>% 
  mutate(got_drafted = case_when(!is.na(draft_year) & draft_year < 2020
                                 ~ 'Yes',
                                 TRUE ~ 'No'))
ohl_filtered %>% # doesn't work
  filter(!is.na(ppg), !is.na(gp), !is.na(g), !is.na(a), !is.na(pim),
         !is.na(pm)) %>% View()

ohl_filtered2 <- ohl_filtered



# from bart -> make treatment binary -- DOESN'T WORK IN THIS CODE
ohl_filtered2 <- ohl_filtered2 %>% 
  mutate(treatment = ifelse(treatment == "Played", 1, 0),
         treatment = as.integer((treatment)))

ohl_filtered2 <- ohl_filtered %>% 
  mutate(treatment = case_when(treatment == "Played" ~ 1,
                               treatment == "Didn't Play" ~ 0))

# convert to factors - DONT NEED?
ohl_filtered2$season <- as.factor(ohl_filtered2$season)
ohl_filtered2$position <- as.factor(ohl_filtered2$position)
ohl_filtered2$got_drafted <- as.factor(ohl_filtered2$got_drafted)
ohl_filtered2$treatment <- as.factor(ohl_filtered2$treatment)

# ignore - first attempts -------------------------------------------------

# tried with all leagues
match1 <- 
  matchit(covid_played ~ season + league + gp + g + a + pim + pm + name +
            position + ppg + got_drafted,
          data = ohl_filtered, method = "nearest",
          distance = "gam",
          replace = FALSE,
          ratio = 2)
# didn't include draft_year, round, etc b/c so many NAs
plot(match1, type = "jitter", interactive = FALSE) # not good
plot(summary(match1)) # this is really messed up

match2 <- 
  matchit(treatment ~ season + league + gp + g + a + pim + pm + name +
            position + ppg + got_drafted,
          data = ohl_filtered2, method = "nearest",
          distance = "mahalanobis",
          replace = FALSE,
          ratio = 2)

# matching ----------------------------------------------------------------


## ignore

# use other type of distance
first_match <- 
  matchit(treatment ~ gp_19_20 + position + pts_19_20 + ppg_19_20 + 
            age_continuous + pm_19_20 + gp_covid + got_drafted,
          data = ohl_filtered, method = "nearest",
          distance = "mahalanobis",
          replace = FALSE,
          ratio = 2)
summary(first_match)
plot(summary(first_match)) # love plot
plot(first_match, type = "jitter", interactive = FALSE)

try2 <- matchit(played_covid ~ gp_19_20 + position + pts_19_20 + ppg_19_20 + 
          age_continuous + pm_19_20 + gp_covid + got_drafted,
        data = ohl_update, method = "nearest",
        distance = "mahalanobis",
        replace = FALSE,
        ratio = 2)
summary(try2)

# re-fit
first_match_refit <- match.data(first_match)
table(first_match_refit$treatment)

first_logit_glm <- glm(treatment ~ gp_19_20 + position + pts_19_20 + 
                         ppg_19_20 + age_continuous + pm_19_20 + gp_covid + 
                         got_drafted, 
                       data = ohl_filtered,
                  family = "binomial")
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred

## ignore 

# or with gam:
first_logit_gam <- gam(treatment ~ s(kick_distance), 
                       data = nfl_fg_attempts,
                      family = "binomial")

gam_propensity_match <- 
  matchit(treatment ~ gp_19_20 + position + pts_19_20 + ppg_19_20 + 
            age_continuous + pm_rank_19_20, 
          data = ohl_filtered2, method = "nearest",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 1)
summary(gam_propensity_match)
plot(gam_propensity_match, type = "jitter", interactive = FALSE)
plot(summary(gam_propensity_match))

plot(gam_propensity_match, type = "qq", interactive = FALSE,
     which.xs = c("gp_19_20", "ppg_19_20", "pm_rank_19_20"))
## why is this only showing gp?
## can it not take categorical?


# does not have pts_19_20
gam_propensity_match2 <- 
  matchit(treatment ~ gp_19_20 + position + ppg_19_20 + 
            age_continuous + pm_rank_19_20, 
          data = ohl_filtered2, method = "nearest",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 1)
summary(gam_propensity_match2) # lowest Std. Pair Dist.
plot(gam_propensity_match2, type = "jitter", interactive = FALSE)
plot(summary(gam_propensity_match2))

### can't tell which is better - first or second 
### second has better love plot

# does not have pm_19_20
gam_propensity_match3 <- 
  matchit(treatment ~ gp_19_20 + position + ppg_19_20 + 
            age_continuous, 
          data = ohl_filtered2, method = "nearest",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 1)
summary(gam_propensity_match3)
plot(gam_propensity_match3, type = "jitter", interactive = FALSE)
plot(summary(gam_propensity_match3))
# the worst match

# cobalt package ----------------------------------------------------------

bal.plot(gam_propensity_match, var.name = "distance") # look into
bal.plot(gam_propensity_match2, var.name = "distance")
bal.plot(gam_propensity_match3, var.name = "distance")

bal.plot(gam_propensity_match, var.name = "gp_19_20")
bal.plot(gam_propensity_match, var.name = "pts_19_20") # look into
bal.plot(gam_propensity_match, var.name = "ppg_19_20") # look into
bal.plot(gam_propensity_match, var.name = "age_continuous")
bal.plot(gam_propensity_match, var.name = "pm_19_20")
bal.plot(gam_propensity_match, var.name = "position")


# post 7/25 meeting -------------------------------------------------------

# just matched data
gam_matched <- match.data(gam_propensity_match)
table(gam_matched$treatment)
matched_model <- glm(treatment ~ gp_19_20 + position + pts_19_20 + 
                       ppg_19_20 + age_continuous + pm_rank_19_20, 
                     data = gam_matched, family = "binomial")
summary(matched_model)
plot(matched_model, which = 1)
coeftest(matched_model, vcov. = vcovCL, cluster = ~subclass)

new_match <- lm(ppg_21_22 ~ position + ppg_19_20 + treatment + drafted + 
                   gp_21_22 + age_continuous + pts_19_20,
                data = gam_matched)

gam_matched2 <- match.data(gam_propensity_match2)
matched_model2 <- glm(treatment ~ gp_19_20 + position + 
                        ppg_19_20 + age_continuous + pm_rank_19_20, 
                      data = gam_matched2, family = "binomial")
summary(matched_model2)
plot(matched_model2, which = 1)
coeftest(matched_model2, vcov. = vcovCL, cluster = ~subclass)

# trying "optimal" method

opt_propensity_match2 <- 
  matchit(treatment ~ gp_19_20 + position + ppg_19_20 + 
            age_continuous + pm_rank_19_20, 
          data = ohl_filtered2, method = "optimal",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 1)
summary(opt_propensity_match2)
# a lot better std. pair dist. than the nearest one
plot(opt_propensity_match2, type = "jitter", interactive = FALSE)
# better than first optimal one
plot(summary(opt_propensity_match2))

# optimal matched data

opt_matched2 <- match.data(opt_propensity_match2)
matched_opt_model2 <- glm(treatment ~ gp_19_20 + position + 
                           ppg_19_20 + age_continuous + pm_rank_19_20, 
                         data = opt_matched2, family = "binomial")
summary(matched_opt_model2)
plot(matched_opt_model2, which = 1) 
# the best of the options
coeftest(matched_opt_model2, vcov. = vcovCL, cluster = ~subclass)


# final optimal model -----------------------------------------------------

opt_propensity_match <- 
  matchit(treatment ~ gp_19_20 + position + pts_19_20 + ppg_19_20 + 
            age_continuous + pm_rank_19_20, 
          data = ohl_filtered2, method = "optimal",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 1)
summary(opt_propensity_match)
# a lot better std. pair dist. than the nearest one
plot(opt_propensity_match, type = "jitter", interactive = FALSE)
plot(summary(opt_propensity_match))

# distribution plots of individual variables
bal.plot(opt_propensity_match, var.name = "distance",
         colors = c("goldenrod1", "dodgerblue"))
bal.plot(opt_propensity_match, var.name = "distance")
bal.plot(opt_propensity_match, var.name = "gp_19_20")
bal.plot(opt_propensity_match, var.name = "position")
bal.plot(opt_propensity_match, var.name = "pts_19_20")
bal.plot(opt_propensity_match, var.name = "ppg_19_20")
bal.plot(opt_propensity_match, var.name = "age_continuous")
bal.plot(opt_propensity_match, var.name = "pm_rank_19_20")

# take just matched data
opt_matched <- match.data(opt_propensity_match)
matched_opt_model <- glm(treatment ~ gp_19_20 + position + pts_19_20 + 
                           ppg_19_20 + age_continuous + pm_rank_19_20, 
                         data = opt_matched, family = "binomial")
summary(matched_opt_model)
plot(matched_opt_model, which = 1)
coeftest(matched_opt_model, vcov. = vcovCL, cluster = ~subclass)

# linear regression
another_match <- lm(ppg_21_22 ~ position + ppg_19_20 + treatment + 
                      gp_21_22 + age_continuous + pts_19_20,
                    data = opt_matched)
# changed to opt_match_lm 
summary(another_match)
coeftest(another_match, vcov. = vcovCL, cluster = ~subclass)
plot(another_match, which = 1)
coeftest(matched_opt_model, vcov. = vcovCL, cluster = ~subclass)

# take out points
another_match2 <- lm(ppg_21_22 ~ position + ppg_19_20 + treatment + 
                      gp_21_22 + age_continuous,
                    data = opt_matched)
summary(another_match2)
plot(another_match2, which = 1)


recent %>% filter(name == "Shane Wright") %>% View()
recent %>% filter(name == "Brandt Clarke") %>% View()




