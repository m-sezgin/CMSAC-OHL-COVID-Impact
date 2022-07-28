# purpose: BART and matching


# load packages -----------------------------------------------------------

library(tidyverse)
library(BART)
library(tidytreatment)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(RColorBrewer)


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

# load rds files
var_select_bart <- readRDS("var_select_bart.rds")
prop_bart <- readRDS("prop_bart.rds")
te_model <- readRDS("te_model.rds")

# fit variable selection model --------------------------------------------
# step 1

var_select_bart <- wbart(x.train = dply::select(ohl_filtered2, -draft_year, -round),
                         y.train = pull(ohl_filtered, ppg_19_20),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000) # don't use - updated below
var_select_bart <- wbart(x.train = dply::select(filtered, gp_19_20, pts_19_20),
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

var_select_bart <- wbart(x.train = dplyr::select(ohl_update, gp_19_20, pts_19_20, 
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

prop_bart <- pbart(x.train = dplyr::select(ohl_update, all_of(var_select)),
                   y.train = pull(ohl_update, treatment),
                   nskip = 2000,
                   ndpost = 5000)

# store propensity score in data
ohl_update$prop_score <- prop_bart$prob.train.mean


# fit the treatment effect model ------------------------------------------
# step 4

te_model <- wbart(x.train = dplyr::select(ohl_update, gp_19_20, pts_19_20, 
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
  dplyr::select(treatment, is_forward) %>% 
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
                    newdata = dplyr::select(ohl_update, gp_19_20, pts_19_20, 
                                     age_continuous, pm_relative_19_20, 
                                     pm_rank_19_20, pm_19_20, is_forward, 
                                     is_drafted, treatment, prop_score))

# histogram of treatment effect (all draws)
posterior_treat_eff %>% 
  ggplot() +
  geom_histogram(aes(x = cte), bins = 50, color = "white") +
  theme_bw() + ggtitle("Histogram of treatment effect (all draws)")

# histogram of treatment effect (mean for each subject)
posterior_treat_eff %>% summarise(cte_hat = mean(cte)) %>%
  ggplot() +
  geom_histogram(aes(x = cte_hat), bins = 60, colour = "white") + 
  theme_bw() + 
  ggtitle("Histogram of treatment effect (median for each subject)")

# get the ATE and ATT directly:
posterior_ate <- tidy_ate(te_model, treatment = "treatment", 
                          newdata = ohl_update)
## made R abort
posterior_att <- tidy_att(te_model, treatment = "treatment", 
                          newdata = ohl_update)
## didn't try running cause I didn't want to crash R

# sample based (using data from fit) conditional treatment effects, 
# posterior draws
posterior_treat_eff_on_treated <- 
  treatment_effects(te_model, treatment = "treatment", newdata = ohl_update, 
                    subset = "treated") 
## also made R abort

# posterior CIs of the CATEs
posterior_treat_eff %>% select(-treatment) %>% point_interval() %>%
  arrange(cte) %>% mutate(.orow = 1:n()) %>% 
  ggplot() + 
  geom_interval(aes(x = .orow, y= cte, ymin = .lower, ymax = .upper)) +
  geom_point(aes(x = .orow, y = cte), shape = "circle open", alpha = 0.5) + 
  ylab("Median posterior CATE for each subject (95% CI)") +
  theme_bw() + coord_flip() + scale_colour_brewer() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "none")

# CATEs varying over position (is_forward) - haven't updated
posterior_treat_eff %>%
  left_join(tibble(is_forward = ohl_update$is_forward, 
                   .row = 1:length(ohl_update$is_forward) ), by = ".row") %>%
  group_by(is_forward) %>%
  ggplot() + 
  stat_halfeye(aes(x = is_forward, y = cte), alpha = 0.7) +
  scale_fill_brewer() +
  theme_bw() + ggtitle("Treatment effect by position (is_forward)")


# investigating variable importance ---------------------------------------

# occurrences of a variable in BART
treatment_interactions <-
  covariate_with_treatment_importance(te_model, treatment = "treatment")

treatment_interactions %>% 
  ggplot() + 
  geom_bar(aes(x = variable, y = avg_inclusion), stat = "identity") +
  theme_bw() + 
  ggtitle("Important variables interacting with treatment ('treatment')") + 
  ylab("Inclusion counts") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

variable_importance <-
  covariate_importance(te_model)

variable_importance %>% 
  ggplot() + 
  geom_bar(aes(x = variable, y = avg_inclusion), stat = "identity") +
  theme_bw() + ggtitle("Important variables overall") +
  ylab("Inclusion counts") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# model checking and convergence ------------------------------------------

# examining model residuals
res <- residual_draws(te_model, response = pull(ohl_update, treatment), 
                      include_newdata = FALSE)
res %>%   
  point_interval(.residual, y, .width = c(0.95) ) %>%
  select(-y.lower, -y.upper) %>%
  ggplot() + 
  geom_pointinterval(aes(x = y, y = .residual, ymin = .residual.lower,  
                         ymax = .residual.upper), alpha = 0.2) +
  scale_fill_brewer() +
  theme_bw() + ggtitle("Residuals vs observations")
# useless

# fit regression
res %>% summarise(.fitted = mean(.fitted), y = first(y)) %>% 
  ggplot(aes(x = y, y = .fitted)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() + ggtitle("Observations vs fitted")
# useless

# Q-Q plot
res %>% summarise(.residual = mean(.residual)) %>%
  ggplot(aes(sample = .residual)) + 
  geom_qq() + 
  geom_qq_line() + 
  theme_bw() + ggtitle("Q-Q plot of residuals")
# weird

# save as RDS files -------------------------------------------------------

# saveRDS(var_select_bart, "../CMSAC-NHL-COVID-Impact/var_select_bart.rds")
# saveRDS(prop_bart, "../CMSAC-NHL-COVID-Impact/prop_bart.rds")
# saveRDS(te_model, "../CMSAC-NHL-COVID-Impact/te_model.rds")

