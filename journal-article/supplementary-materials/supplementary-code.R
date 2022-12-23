# PURPOSE: Provide supplementary graphs, models, tables, etc for journal paper

# Packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(lubridate)
library(ggridges)
library(RColorBrewer)
library(kableExtra)
library(DHARMa)
library(merTools)
library(BART)
library(tidytreatment)
library(tidybayes)
library(MatchIt)
library(mgcv)
library(cobalt)
library(optmatch)
library(lme4)
library(cowplot)
library(ggthemes)
library(gridGraphics)
library(ggmosaic)
library(gridExtra)

# Data --------------------------------------------------------------------

ohl_filtered <- read_csv("../data/ohl_filtered.csv")

## Data integrity

# sam's data
ohl <- read_csv("../data/sams_ohl_data_request.csv")

# sam's data 2019 and 2021
ohl_19 <- ohl %>%
  filter(season == "2019-2020", league == "OHL")
ohl_21 <- ohl %>%
  filter(season == "2021-2022", league == "OHL")

# team level stats
ohl_team_19 <- read_csv("../../data/OHL_team_stats_19_20.csv")
ohl_team_21 <- read_csv("../../data/OHL_team_stats_21_22.csv")

ohl_team_19 <- ohl_team_19 %>%
  dplyr::select(Team, GF) %>%
  mutate(Team = case_when(Team == "Ottawa 67'S" ~ "Ottawa 67's",
                          TRUE ~ Team)
  ) %>%
  rename(c("team_name" = "Team", "true_goals_19" = "GF"))

ohl_team_21 <- ohl_team_21 %>%
  dplyr::select(Team, GF) %>%
  rename(c("team_name" = "Team", "true_goals_21" = "GF"))

sam_team_goals_19 <- ohl_19 %>%
  group_by(team_name) %>%
  summarize(goals_19 = sum(g)) %>%
  arrange(desc(goals_19))

sam_team_goals_21 <- ohl_21 %>%
  group_by(team_name) %>%
  summarize(goals_21 = sum(g)) %>%
  arrange(desc(goals_21))

ohl_combined_19 <- sam_team_goals_19 %>%
  full_join(ohl_team_19, by = "team_name") %>%
  mutate(difference_19 = true_goals_19 - goals_19) #%>%
#summarize(pct = mean(percent))

ohl_combined_21 <- sam_team_goals_21 %>%
  full_join(ohl_team_21, by = "team_name") %>%
  mutate(difference_21 = true_goals_21 - goals_21) #%>%
#summarize(pct = mean(percent))

#mean_pct_both_yrs <- mean(c(ohl_combined_19$percent, ohl_combined_21$percent))

ohl_combined <- ohl_combined_19 %>%
  full_join(ohl_combined_21, by = "team_name")

saveRDS(ohl_combined, "data_integ_table.rds")

# EDA ---------------------------------------------------------------------

# distribution of response
pp_plot <- ggplot(ohl_filtered, aes(x = ppg_21)) +
  geom_density(color = "royalblue3") +
  labs(title = "Distribution of player performance in post-COVID season",
       x = "Player Performance\n(PPG Post-COVID Season)") +
  theme_bw()

#saveRDS(pp_plot, "./supplementary-materials/pp_plot.rds")

# treatment vs player performance
trt_pp_plot <- ggplot(ohl_filtered, aes(x = ppg_21, color = treatment)) +
  geom_density() +
  labs(title = "Players who played during COVID season generally had higher PPG in\npost-COVID season",
       x = "Player Performance\n(PPG Post-COVID Season)",
       color = "COVID season") +
  scale_color_manual(values = c("royalblue3", "darkgoldenrod3")) +
  theme_bw()

#saveRDS(trt_pp_plot, "./supplementary-materials/trt_pp_plot.rds")

prelim_plots <- plot_grid(pp_plot, trt_pp_plot, ncol = 1)

saveRDS(prelim_plots, "./supplementary-materials/prelim_plots.rds")

## Age

# age vs player performance
age_pp_plot <- ggplot(ohl_filtered, aes(x = age_continuous, y = ppg_21)) +
  geom_point(alpha = .5) +
  labs(title = "Weak relationship between player performance and age",
       x = "Age in Pre-COVID Season",
       y = "Player Performance\n(PPG Post-COVID Season)") +
  theme_bw() +
  geom_smooth(method = "lm") 

#saveRDS(age_pp_plot, "age_pp_plot.rds")

# age vs treatment
age_trt_plot <- ggplot(ohl_filtered, aes(x = age_continuous, color = treatment)) +
  geom_density() +
  labs(title = "Similar age distributions for treated and untreated players",
       x = "Age in Pre-COVID Season",
       color = "COVID Season") +
  scale_color_manual(values = c("royalblue3", "darkgoldenrod3")) +
  theme_bw()

#saveRDS(age_trt_plot, "age_trt_plot.rds")

age_plots <- plot_grid(age_pp_plot, age_trt_plot, ncol = 1)

saveRDS(age_plots, "./supplementary-materials/age_plots.rds")

## Games Played (GP)

# gp vs player performance
gp_pp_plot <- ggplot(ohl_filtered, aes(x = gp_21, y = ppg_21)) +
  geom_point(alpha = .5) +
  labs(title = "Positive linear relationship between player performance and GP in \npost-COVID season",
       x = "GP in Post-COVID Season", 
       y = "Player Performance\n(PPG Post-COVID Season)") +
  theme_bw() +
  geom_smooth(method = "lm")

#saveRDS(gp_pp_plot, "gp_pp_plot.rds")

# gp vs treatment
gp_trt_plot <- ggplot(ohl_filtered, aes(x = gp_21, color = treatment)) +
  geom_density() +
  labs(title = "Skaters who played during COVID season played more games\nin post-COVID season",
       x = "GP in Post-COVID Season",
       color = "COVID Season") +
  scale_color_manual(values = c("royalblue3", "darkgoldenrod3")) +
  theme_bw() 

#saveRDS(gp_trt_plot, "gp_trt_plot.rds")

gp_plots <- plot_grid(gp_pp_plot, gp_trt_plot, ncol = 1)

saveRDS(gp_plots, "./supplementary-materials/gp_plots.rds")

## Previous Player Performance

# previous player performance vs player performance
ppp_pp_plot <- ggplot(ohl_filtered, aes(x = ppg_19, y = ppg_21)) +
  geom_point(alpha = .5) +
  labs(title = "Positive linear relationship between post-COVID season player performance \nand pre-COVID season player performance",
       x = "Previous Player Performance (PPG in pre-COVID Season)",
       y = "Player Performance (PPG Post-COVID Season)") +
  geom_smooth(method = "lm") +
  theme_bw()

#saveRDS(pq_pp_plot, "ppp_pp_plot.rds")

# previous performance vs treatment
ppp_trt_plot <- ggplot(ohl_filtered, aes(x = ppg_19, color = treatment)) +
  geom_density() +
  labs(title = "Higher performing players more likely to play during the COVID season",
       x = "Previous Player Performance (PPG in Pre-COVID Season)",
       color = "COVID Season") +
  scale_color_manual(values = c("royalblue3", "darkgoldenrod3")) +
  theme_bw()

#saveRDS(pq_pp_plot, "ppp_trt_plot.rds")

ppp_plots <- plot_grid(gp_pp_plot, ppp_trt_plot, ncol = 1)

saveRDS(ppp_plots, "./supplementary-materials/ppp_plots.rds")

## Position

# position vs ppg
pos_pp_plot <- ggplot(ohl_filtered) +
  geom_density(aes(x = ppg_21, color = position)) +
  labs(title = "Post-COVID season player performance generally greater for forwards",
       x = "Player Performance (PPG in Post-COVID Season)",
       color = "Position") +
  scale_color_manual(values = c("royalblue3", "darkgoldenrod3")) +
  theme_bw()

#saveRDS(pos_pp_plot, "./supplementary-materials/pos_pp_plot.rds")

# position vs treatment
# pos_trt_plot <- function() {
#   mosaicplot(table("Position" = ohl_filtered$position,
#                 "COVID Season" = ohl_filtered$treatment),
#            main = "Player position does not significantly influence probability\nof playing during COVID Season",
#            shade = TRUE)
#           }

pos_trt_plot <- ggplot(ohl_filtered) +
  geom_mosaic(aes(x = product(treatment, position))) +
  labs(title = "Player position does not significantly influence probability\nof playing during COVID Season",
       x = "Position",
       y = "COVID Season") +
  theme_bw()

pos_plots <- plot_grid(pos_pp_plot, pos_trt_plot, ncol = 1)

saveRDS(pos_plots, "./supplementary-materials/pos_plots.rds")

# Models ------------------------------------------------------------------

### Regression

## Baseline

baseline_mod <- lm(ppg_21 ~ 1, data = ohl_filtered)

## OLS

ohl_mlr <- lm(ppg_21 ~ position + ppg_19 + treatment + gp_21 + age_continuous, data = ohl_filtered)
summary(ohl_mlr)

## OLS Interaction

ohl_mlr_interaction <- lm(ppg_21 ~ position*ppg_19 + age_continuous*ppg_19 + ppg_19*treatment + gp_21, data = ohl_filtered)
#plot(ohl_mlr_interaction, which = c(1, 2))
#summary(ohl_mlr_interaction)

# is the full model significantly different from the nested?
anova(ohl_mlr, ohl_mlr_interaction)
# no.

## Gamma simple

# shift ppg by .001 for gamma regression
ohl_filtered <- ohl_filtered %>%
  mutate(ppg_alt = ppg_21 + .001)

ohl_glm_simple <- glm(ppg_alt ~ position + ppg_19 + treatment + drafted_19 + gp_21 + age_continuous, data = ohl_filtered, family = Gamma)

## Gamma interaction

ohl_glm_interaction <- glm(ppg_alt ~  position*ppg_19 + age_continuous*ppg_19 + ppg_19*treatment + gp_21,
                           data = ohl_filtered,
                           family = Gamma)

## OLS final

ohl_mlr_final <- lm(ppg_21 ~ position + ppg_19 + treatment + gp_21 + age_continuous, data = ohl_filtered)
#summary(ohl_mlr_final)

## Mixed effects

ohl_filtered <- ohl_filtered %>%
  mutate(treatment_numeric = ifelse(treatment == "Played", 1, 0))

ohl_lmer <- lmer(ppg_21 ~ position + ppg_19 + gp_21 + age_continuous + treatment_numeric + (treatment_numeric|covid_league),
                 data = ohl_filtered)
#summary(ohl_lmer)

# league_effects <- REsim(ohl_lmer)
# 
# league_effects %>%
#   as_tibble() %>%
#   group_by(groupFctr) %>%
#   arrange(desc(mean)) %>%
#   slice(1:5, (n() - 4):n()) %>%
#   ggplot(aes(x = reorder(groupID, mean))) +
#   geom_point(aes(y = mean)) +
#   geom_errorbar(aes(ymin = mean - 2 * sd,
#                     ymax = mean + 2 * sd)) +
#   facet_wrap(~groupFctr, ncol = 1, scales = "free_y") +
#   geom_vline(xintercept = 0, linetype = "dashed",
#              color = "red") +
#   coord_flip() +
#   theme_bw() +
#   labs(title = "League effect sizes")
# plotREsim(league_effects, labs = TRUE)

### Causal models

## Matching

opt_cov_match <- 
  matchit(treatment_numeric ~ gp_21 + position + pts_19 + ppg_19 + 
            age_continuous + pm_rank_19, 
          data = ohl_filtered, method = "optimal",
          distance = "gam",
          replace = FALSE, # do not reuse controls
          ratio = 1)
plot(opt_cov_match, type = "jitter", interactive = FALSE)

opt_matched <- match.data(opt_cov_match)

opt_match_lm <- lm(ppg_21 ~ position + ppg_19 + treatment + gp_21 + age_continuous,
                   data = opt_matched)

#plot(opt_match_lm, which = 1)
#summary(opt_match_lm)

## Bart

ohl_numeric <- ohl_filtered %>%
  mutate(treatment = ifelse(treatment == "Played", 1, 0),
         treatment = as.integer((treatment)),
         is_forward = case_when(position == "F" ~ 1,
                                position == "D" ~ 0),
         is_drafted = case_when(drafted_19 == "Yes" ~ 1,
                                drafted_19 == "No" ~ 0)
  ) %>%
  as.data.frame()

# variable selection model
var_select_bart_nodr <- wbart(x.train = dplyr::select(ohl_numeric, gp_19, gp_21,
                                                      pts_19, age_continuous,
                                                      pm_rank_19, pm_relative_19, pm_19,
                                                      is_forward, ppg_19),
                              y.train = pull(ohl_numeric, ppg_21),
                              sparse = TRUE,
                              ntree = 25,
                              ndpost = 50)

# variable selection
covar_ranking_nodr <- covariate_importance(var_select_bart_nodr)
var_select_nodr <- covar_ranking_nodr %>%
  filter(avg_inclusion >= quantile(avg_inclusion, 0.5)) %>%
  pull(variable)

# probability of being treated
prop_bart_nodr <- pbart(x.train = dplyr::select(ohl_numeric,
                                                all_of(var_select_nodr)),
                        y.train = pull(ohl_numeric, treatment),
                        nskip = 2000,
                        ndpost = 5000)
ohl_numeric$prop_score <- prop_bart_nodr$prob.train.mean

# treatment effect model
te_model_nodr <- wbart(x.train = dplyr::select(ohl_numeric, gp_19, pts_19,
                                               age_continuous, pm_relative_19,
                                               pm_19, is_forward,
                                               ppg_19, treatment, prop_score),
                       # need to include treatment and prop score
                       y.train = pull(ohl_numeric, ppg_21),
                       nskip = 10000L,
                       ndpost = 200L,
                       keepevery = 100L)

posterior_fitted_nodr <- fitted_draws(te_model_nodr, value = "fit",
                                      include_newdata = FALSE)
posterior_fitted_nodr
posterior_pred_nodr <- predicted_draws(te_model_nodr, include_newdata = FALSE)

posterior_means <- posterior_fitted_nodr |>
  group_by(.row) |>
  summarize(posterior_mean = mean(fit))

bart_residuals <- ohl_numeric$ppg_21-posterior_means$posterior_mean

posterior_fitted_nodr |>
  filter(.row < 10) |>
  ggplot(aes(y = fit, x = factor(.row), fill = factor(.row))) +
  geom_boxplot() +
  geom_point(data = posterior_means[1:9,], 
             aes(y = posterior_mean), color = 'red')

# Results -----------------------------------------------------------------

### RMSE table

#rmse_baseline <- sqrt(mean(ohl_filtered$ppg_21 - mean(ohl_filtered$ppg_21)))
rmse_baseline <- sqrt(mean(baseline_mod$residuals^2))
rmse_ols <- sqrt(mean(ohl_mlr$residuals^2))
rmse_ols_int <- sqrt(mean(ohl_mlr_interaction$residuals^2))
rmse_gamma_sim <-sqrt(mean(ohl_glm_simple$residuals^2))
rmse_gamma_int <- sqrt(mean(ohl_glm_interaction$residuals^2))
rmse_lmer <- sqrt(mean(residuals(ohl_lmer)^2))
rmse_matched <- sqrt(mean(opt_match_lm$residuals^2))
rmse_bart <- sqrt(mean(bart_residuals^2))

rmse_table <- tibble(model = c("BART", "mixed effects", "OLS interaction", "OLS", "intercept-only", "matched", "gamma simple", "gamma interaction"),
       RMSE = c(rmse_bart, rmse_lmer, rmse_ols_int, rmse_ols, rmse_matched, rmse_baseline, rmse_gamma_sim, rmse_gamma_int))

saveRDS(rmse_table, "./supplementary-materials/rmse_table.rds")

### Regression

ols_plot <- ohl_filtered %>%
  mutate(pred_vals = predict(ohl_mlr_final)) %>%
  ggplot(aes(x = pred_vals,
             y = ppg_21)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "red",
              size = 2) +
  theme_bw() +
  labs(title = "Model fit: Actual PPG in post-COVID season vs predicted PPG in\npost-COVID season",
       x = "Predicted PPG in post-COVID season",
       y = "Actual PPG in Post-COVID Season")

saveRDS(ols_plot, "./supplementary-materials/ols_plot.rds")

shane_demo <- ohl_filtered %>%
  mutate(ols_pred = ohl_mlr_final$fitted.values,
         ols_resid = ohl_mlr_final$residuals) %>%
  filter(player_id == 526239) %>%
  dplyr::select(treatment, position, ppg_19, gp_21, age_continuous, ppg_21, ols_pred)

saveRDS(shane_demo, "./supplementary-materials/shane_demo.rds")

### BART

# sample based (using data from fit) conditional treatment effects,
# posterior draws
posterior_treat_eff <-
  treatment_effects(te_model_nodr, treatment = "treatment",
                    # the dataset here needs to match the BART data EXACTLY
                    # which is really annoying...
                    newdata = dplyr::select(ohl_numeric, gp_19, pts_19,
                                            age_continuous, pm_relative_19,
                                            pm_19, is_forward,
                                            ppg_19, treatment, prop_score))

## All Draws

# histogram of treatment effect (all draws)
bart_draws <- posterior_treat_eff %>%
  ggplot() +
  geom_histogram(aes(x = cte), bins = 50, color = "white") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  theme_bw() + ggtitle("Treatment effect (all draws)")

#saveRDS(bart_draws, "./supplementary-materials/bart_draws.rds")

## Mean For Each Subject

# histogram of treatment effect (mean for each subject)
bart_means <- posterior_treat_eff %>% summarise(cte_hat = mean(cte)) %>%
  ggplot() +
  geom_histogram(aes(x = cte_hat), bins = 60, colour = "white") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  theme_bw() +
  ggtitle("Treatment effect (mean for each subject)")

#saveRDS(bart_means, "./supplementary-materials/bart_means.rds")

bart_plots <- plot_grid(bart_draws, bart_means, ncol = 1)

saveRDS(bart_plots, "./supplementary-materials/bart_plots.rds")

## CIs of the CATEs

# posterior CIs of the CATEs
bart_cates <- posterior_treat_eff %>% dplyr::select(-treatment) %>% point_interval() %>%
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

saveRDS(bart_cates, "./supplementary-materials/bart_cates.rds")

# Limitations -------------------------------------------------------------

### Regression

ols_data <- ohl_filtered |>
  mutate(ols_pred = ohl_mlr_final$fitted.values,
         ols_resid = ohl_mlr_final$residuals)

ols_res_plot <- ggplot(ols_data, aes(x = ols_pred, y = ols_resid)) +
  geom_point()+
  stat_smooth(se = FALSE) +
  labs(title = "OLS Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_bw()
ols_qq_plot <- ggplot(ols_data, aes(sample = ppg_21)) + 
  stat_qq() +
  geom_qq_line(linetype = "dashed") +
  labs(title = "OLS Normal Q-Q",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_bw()


ols_lim_plots <- cowplot::plot_grid(ols_res_plot, ols_qq_plot, ncol = 1)

saveRDS(ols_lim_plots, "./supplementary-materials/ols_lim_plots.rds")
