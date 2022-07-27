# purpose: BART and matching


# load packages -----------------------------------------------------------

library(tidyverse)
library(BART)
library(tidytreatment)
library(dplyr)
library(tidybayes)
library(ggplot2)


# load data ---------------------------------------------------------------

library(readr)
ohl_filtered <- read_csv("ohl_filtered.csv")

ohl_filtered <- ohl_filtered %>% 
  mutate(got_drafted = case_when(!is.na(draft_year) & draft_year < 2020
                                 ~ 'Yes',
                                 TRUE ~ 'No'))

table(ohl_filtered$treatment)
# Didn't Play     Played
#         152         67

# convert treatment to binary indicator:
ohl_filtered <- ohl_filtered %>% 
  mutate(treatment = ifelse(treatment == "Played", 1, 0),
         treatment = as.integer((treatment)))
## is using ifelse (over case_when) just a preference? 
ohl_filtered2 <- ohl_filtered
# fit variable selection model --------------------------------------------
# step 1

var_select_bart <- wbart(x.train = select(ohl_filtered2, -draft_year, -round),
                         y.train = pull(ohl_filtered, ppg_19_20),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000) # don't use - updated below
var_select_bart <- wbart(x.train = select(filtered, gp_19_20, pts_19_20),
                         y.train = pull(ohl_filtered, ppg_19_20),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000) # don't use - updated below
# WORKS!!!!

ohl_filtered <- as.data.frame(ohl_filtered)

# convert categorical variables to binary
ohl_update <- ohl_filtered %>% 
  mutate(is_forward = case_when(position == "F" ~ 1, 
                                position == "D" ~ 0)) %>% 
  mutate(is_drafted = case_when(got_drafted == "Yes" ~ 1,
                                got_drafted == "No" ~ 0)) %>% 
  # mutate(played_covid = case_when(treatment == "TRUE" ~ 1,
  #                                treatment == "FALSE" ~ 0)) %>% 
  mutate(s_played = case_when(season == "2021-2022" ~ 1,
                              season == "2019-2020" ~ 0)) %>% 
  as.data.frame()

var_select_bart <- wbart(x.train = select(ohl_update, gp_19_20, pts_19_20, 
                                         age_continuous, pm_relative_19_20, 
                                         pm_rank_19_20, pm_19_20, is_forward, 
                                         is_drafted),
                        y.train = pull(ohl_update, ppg_19_20),
                        sparse = TRUE,
                        nskip = 2000,
                        ndpost = 5000)


# variable selection ------------------------------------------------------
# step 2

covar_ranking <- covariate_importance(var_select_bart)
var_select <- covar_ranking %>% 
  filter(avg_inclusion >= quantile(avg_inclusion, 0.5)) %>% 
  pull(variable)

# do I need the change categorical variables to just one step?


# fit a propensity score model --------------------------------------------
# step 3

prop_bart <- pbart(x.train = select(ohl_update, all_of(var_select)),
                   y.train = pull(ohl_update, treatment),
                   nskip = 2000,
                   ndpost = 5000)

# store propensity score in data
ohl_update$prop_score <- prop_bart$prob.train.mean


# fit the treatment effect model ------------------------------------------
# step 4

te_model <- wbart(x.train = select(ohl_update, gp_19_20, pts_19_20, 
                                   age_continuous, pm_relative_19_20, 
                                   pm_rank_19_20, pm_19_20, is_forward, 
                                   is_drafted, treatment, prop_score),
                                   # need to include treatment and prop score
                  y.train = pull(ohl_update, ppg_19_20),
                  nskip = 10000L,
                  ndpost = 200L,
                  keepevery = 100L)


# extract the posterior ---------------------------------------------------

posterior_fitted <- fitted_draws(te_model, value = "fit",
                                 include_newdata = FALSE)
posterior_fitted

# function to tidy predicted draws and add random normal noise by default
posterior_pred <- predicted_draws(te_model, include_newdata = FALSE)


# plots using tidybayes ---------------------------------------------------

treatment_var_and_c1 <- ohl_update %>% 
  select(treatment, is_forward) %>% 
  mutate(.row = 1:n(), treatment = as.factor(treatment))

posterior_fitted %>% 
  left_join(treatment_var_and_c1, by = ".row") %>% 
  ggplot() +
  stat_halfeye(aes(x = treatment, y = fit)) +
  facet_wrap(~is_forward, labeller = 
               as_labeller( function(x) paste("is_forward =",x) ) ) +
  xlab("Treatment (played during covid)") + ylab("Posterior predicted value") +
  theme_bw() + 
  ggtitle("Effect of treatment with 'is_forward' on posterior fitted value")


# calculate treatment effects ---------------------------------------------

# sample based (using data from fit) conditional treatment effects,
# posterior draws
posterior_treat_eff <- 
  treatment_effects(te_model, treatment = "treatment",
                    # the dataset here needs to match the BART data EXACTLY
                    # which is really annoying...
                    newdata = select(ohl_update, gp_19_20, pts_19_20, 
                                     age_continuous, pm_relative_19_20, 
                                     pm_rank_19_20, pm_19_20, is_forward, 
                                     is_drafted, treatment, prop_score))

# histogram of treatment effect (all draws)
posterior_treat_eff %>% 
  ggplot() +
  geom_histogram(aes(x = cte), bins = 30, color = "white") +
  theme_bw() + ggtitle("Histogram of treatment effect (all draws)")

