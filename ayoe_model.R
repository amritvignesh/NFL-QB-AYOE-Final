# install.packages("nflfastR")
# install.packages("dplyr")
# install.packages("xgboost")
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("vip")
# install.packages("gt")
# install.packages("gtExtras")
# install.packages("ggtext")

library(nflfastR)
library(dplyr)
library(xgboost)
library(tidyverse)
library(caret)
library(vip)
library(gt)
library(gtExtras)
library(ggtext)

pbp <- load_pbp(2014:2023) # load play by play data 

data <- pbp %>%
  filter(!is.na(air_yards)) %>%
  filter(play_type != "qb_spike") %>%
  mutate(timeouts = ifelse(posteam_type == "home", home_timeouts_remaining, away_timeouts_remaining)) %>%
  mutate(season_final = as.numeric(substring(game_id, 1, 4))) %>%
  filter(!is.na(receiver)) # get only pass attempts with certain air yards and targeted receiver

target_stats <- calculate_player_stats(pbp, weekly = TRUE)

target_stats <- subset(target_stats, player_id %in% data$receiver_id)

target_stats <- target_stats %>%
  select(player_id, player_display_name, season, week, receiving_epa) # receiving EPA for every week within a season

target_stats <- target_stats %>%
  arrange(player_id, week) %>%
  group_by(player_id) %>%
  select(receiver_id = player_id, season, week, receiving_epa) # getting all receiver stats

unique_receivers <- unique(data$receiver_id)

final_target_stats <- data.frame(matrix(nrow = length(unique_receivers), ncol = 4)) 
colnames(final_target_stats) <- c("receiver_id", "season", "week", "receiving_epa")

final_target_stats$receiver_id <- unique_receivers

max_week <- max(target_stats$week)

final_target_stats <- expand.grid(receiver_id = final_target_stats$receiver_id, season = 2014:2023, week = 1:max_week) # creating a grid with all possible weeks and seasons 

final_target_stats <- left_join(final_target_stats, target_stats, by = c("receiver_id", "season", "week")) 

final_target_stats <- final_target_stats %>%
  mutate(receiving_epa = ifelse(is.na(receiving_epa), 0, receiving_epa)) %>%
  arrange(receiver_id, season, week) %>%
  group_by(receiver_id, season) %>%
  mutate(cumulative_receiving_epa = cumsum(receiving_epa) - receiving_epa) # generating cumulative receiving EPA before the game in which the attempt occurred

new_data <- inner_join(data, final_target_stats, by = c("receiver_id", "season_final"="season", "week")) 

new_data <- new_data %>%
  select(passer_id, season_final, season_type, name, posteam, yardline_100, half_seconds_remaining, down, ydstogo, shotgun, no_huddle, qb_dropback, ep, wp, score_differential, cumulative_receiving_epa, air_yards) # data with all features

colnames(new_data)
factor_data <- new_data %>%
  select(-passer_id, -season_final, -name, -posteam)

factor_data$down <- as.factor(factor_data$down) # making factor data into factors
factor_data$shotgun <- as.factor(factor_data$shotgun)
factor_data$no_huddle <- as.factor(factor_data$no_huddle)
factor_data$qb_dropback <- as.factor(factor_data$qb_dropback)
factor_data$season_type <- as.factor(factor_data$season_type)

dummy <- dummyVars(" ~ .", data = factor_data)
final_data <- data.frame(predict(dummy, newdata = factor_data)) # creating dummy variables with binary values for categories

final_data <- cbind(new_data, final_data) 

final_data <- final_data[,-c(3, 8, 10:12, 20:21, 26, 33:37)]

xgboost_train <- final_data %>%
  filter(season_final < 2021) # train data: 2014 - 2020

xgboost_test <- final_data %>%
  filter(season_final >= 2021) # test data: 2021 - 2023

labels_train <- as.matrix(xgboost_train[, 12]) # air yards is the label
xgboost_trainfinal <- as.matrix(xgboost_train[, c(5:11, 13:24)]) # all the features
xgboost_testfinal <- as.matrix(xgboost_test[, c(5:11, 13:24)])

ayoe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "reg:squarederror", early_stopping_rounds = 10, max_depth = 6, eta = 0.3) # parameters for xgboost model

vip(ayoe_model) # generating VIP/importance plot

air_yards_predict <- predict(ayoe_model, xgboost_testfinal) 
air_yards <- as.matrix(xgboost_test[,12]) # predicted and actual values for each attempt

air_yards_predictions <- as.data.frame(
  matrix(predict(ayoe_model, as.matrix(final_data[,c(5:11, 13:24)])))
)

all_stats <- cbind(final_data, air_yards_predictions) %>%
  select(passer_id, season = season_final, name, team = posteam, air_yards, pred_air_yards = V1)

all_stats <- all_stats %>%
  group_by(passer_id, season, name) %>%
  summarize(air_yards = sum(air_yards), pred_air_yards = sum(pred_air_yards), ayoe = air_yards - pred_air_yards)

attempts <- data %>%
  mutate(pass_attempt = ifelse(play_type == "pass", 1, 0)) %>%
  group_by(passer_id, season_final) %>%
  summarize(attempts = n())

all_stats <- inner_join(all_stats, attempts, by = c("passer_id", "season" = "season_final"))

all_stats <- all_stats %>%
  mutate(avg_air_yards = air_yards/attempts, avg_pred_air_yards = pred_air_yards/attempts, avg_ayoe = ayoe/attempts) # getting aDOT, expected aDOT, and generating AYOE

test_data <- all_stats %>%
  filter(season >= 2021, attempts >= 250) %>%
  group_by(passer_id) %>%
  arrange(passer_id) 

rosters <- fast_scraper_roster(2021:2023) %>%
  select(season, passer_id = gsis_id, full_name, team, headshot_url) # for visualization

test_data <- left_join(test_data, rosters, by = c("season", "passer_id"))

logos <- teams_colors_logos %>%
  select(team = team_abbr, team_logo_espn) # for visualization

test_data <- left_join(test_data, logos, by = "team") 

epa_stats <- data.frame() 

for (szn in 2021:2023) {
  pbp_szn <- load_pbp(szn)
  epa_stats_szn <- calculate_player_stats(pbp_szn, weekly = FALSE) %>%
    mutate(epa_per_pass = passing_epa/attempts, season = szn) %>%
    select(season, player_id, epa_per_pass)
  epa_stats <- rbind(epa_stats, epa_stats_szn) # getting epa stats for every season
}

test_data <- left_join(test_data, epa_stats, by = c("season", "passer_id"="player_id"))

viz_test_data <- test_data %>% 
  arrange(-avg_ayoe) %>%
  mutate(avg_ayoe = round(avg_ayoe, 3), epa_per_pass = round(epa_per_pass, 3)) %>%
  ungroup() %>%
  select(season, headshot_url, full_name, team_logo_espn, epa_per_pass, avg_ayoe) # table data

viz_data_2023 <- viz_test_data %>% filter(season == 2023) %>% select(-season) # 2023 data

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>nflverse</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

table_2023 <- viz_data_2023 %>% gt() %>%
  gt_img_rows(columns = team_logo_espn, height = 40) %>%
  gt_img_rows(columns = headshot_url, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(headshot_url, full_name, team_logo_espn, epa_per_pass, avg_ayoe)
  ) %>%
  gt_hulk_col_numeric(c(epa_per_pass, avg_ayoe)) %>%
  cols_label(
    headshot_url = md(""),
    full_name = md("**QB**"),
    team_logo_espn = md("**Team**"),
    epa_per_pass = md("**EPA/Pass**"),
    avg_ayoe = md("**AYOE**")
  ) %>%
  tab_header(
    title = "2023 NFL QB aDOT Over Expected",
    subtitle = md("*QBs with ≥ **250** Pass Attempts*")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(full_name, avg_ayoe)
    )
  )

gtsave(table_2023, "table_2023.png", vwidth=1000, vheight=2500, zoom=1)

plot_2023 <- viz_data_2023 %>%
  ggplot(aes(x = avg_ayoe, y = epa_per_pass)) +
  geom_hline(yintercept = mean(viz_data_2023$epa_per_pass), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(viz_data_2023$avg_ayoe), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = "lm") + 
  geom_from_path(aes(path = headshot_url), height = 0.1) +
  labs(x = "AYOE",
       y = "EPA Per Pass",
       title = "Quarterback Efficiency vs Aggressiveness",
       caption = "Data from **nflverse** | Amrit Vignesh | **@avsportsanalyst**") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.caption = element_markdown(hjust = 0.5)) 

ggsave("plot_2023.png", plot_2023, width = 10, height = 6)

three_seasons <- viz_test_data %>%
  group_by(full_name) %>%
  filter(n() == 3)

dir.create("timelines/")

for (qb in unique(three_seasons$full_name)) { # progression for qbs who played from 2021 - 2023 with 250+ attempts each year
  indiv_data <- three_seasons %>%
    filter(full_name == qb) %>%
    ungroup() %>%
    select(-headshot_url, -full_name) %>%
    arrange(season)
  
  headshots <- three_seasons %>%
    filter(season == 2023) %>%
    select(headshot_url, full_name)
  
  indiv_table <- indiv_data %>% gt() %>%
    gt_img_rows(columns = team_logo_espn, height = 40) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(season, team_logo_espn, epa_per_pass, avg_ayoe)
    ) %>%
    gt_hulk_col_numeric(c(epa_per_pass, avg_ayoe)) %>%
    cols_label(
      season = md("**Season**"),
      team_logo_espn = md("**Team**"),
      epa_per_pass = md("**EPA/Pass**"),
      avg_ayoe = md("**AYOE**")
    ) %>%
    tab_header(
      title = add_text_img("", headshots$headshot_url[which(headshots$full_name == qb)], 80),
      subtitle = md("*Progression of **EPA/Pass** and **AYOE** From **2021** to **2023***")
    ) %>%
    tab_source_note(html(caption)) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c(avg_ayoe)
      )
    ) %>%
    tab_caption(md(paste0("**", qb, "** \n")))
  
  gtsave(indiv_table, file.path("timelines/", paste0(qb, ".png")))
}

