# PURPOSE: data integrity check

# Team level stats --------------------------------------------------------

# sam's data
ohl <- read_csv("./data/sams_ohl_data_request.csv")

# sam's data 2019 and 2021
ohl_19 <- ohl %>%
  filter(season == "2019-2020", league == "OHL")
ohl_21 <- ohl %>%
  filter(season == "2021-2022", league == "OHL")

# team level stats
ohl_team_19 <- read_csv("./data/OHL_team_stats_19_20.csv")
ohl_team_21 <- read_csv("./data/OHL_team_stats_21_22.csv")

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

# Player level cross-check attempt ----------------------------------------

# ohl_og <- read_csv("./data/OHL_player_data_2019_20.csv")
#
# ohl_og_noG <- ohl_og %>%
#   filter(Pos != "G") %>%
#   separate(Name, c("last_name", "first_name"), sep = ", ") %>%
#   unite("name", c(first_name, last_name), sep = " ", remove = FALSE) %>%
#   dplyr::select(name)
# 
# diff1 <- setdiff(ohl_og_noG, ohl_19)
# diff2 <- setdiff(ohl_19, ohl_og_noG)
# 
# ohl %>%
#   filter(league == "OHL", season == "1997-1998") %>%
#   group_by(team_name) %>%
#   summarize(team_goals = sum(g)) %>%
#   arrange(desc(team_goals))
# 
# # age?
# ohl %>%
#   filter(league == "OHL")
# 
# ohl_test <- ohl %>%
#   filter(season == "2019-2020", duplicated(player_id)== FALSE) %>%
#   group_by(player_id) %>%
#   mutate(age = trunc((dob %--% as.Date("2020-01-01")) / years(1)),
#          age_continuous = (dob %--% as.Date("2020-01-01")) / years(1),
#          ppg = pts/gp
#   ) %>%
#   ungroup()
# ggplot(ohl_test, aes(x = age_continuous, y = ppg)) + geom_point()