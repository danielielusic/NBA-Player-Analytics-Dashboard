#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
################################################################################
###### Load Libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(hablar)
library(ggplot2)
library(gt)
library(jsonlite)
library(httr)
library(nbastatR)
library(hoopR)
library(ggrepel)
library(ggthemes)
library(gtExtras)
library(ggtext)

################################################################################
##### Load data
## See webscraping.R for web scraping details
## I will try to update the data weekly
load("nbadata.rda")

################################################################################
##### Plot Functions
#### Offensive Overview Page
### Summary Table
create_summary_table <- function(player=player) {
  team_abbr <- advanced %>% 
    filter(player_name == player) %>% 
    pull(team_abbr)
  position <- advanced %>% 
    filter(player_name == player) %>% 
    pull(groupPosition)
  gp <- advanced %>% 
    filter(player_name == player) %>% 
    pull(countGames)
  mpg <- round(advanced %>% 
                 filter(player_name == player) %>% 
                 pull(mpg), 1)
  ppg <- advanced %>% 
    filter(player_name == player) %>% 
    pull(ppg)
  rpg <- advanced %>% 
    filter(player_name == player) %>% 
    pull(rpg)
  apg <- advanced %>% 
    filter(player_name == player) %>% 
    pull(apg)
  pctFG <- advanced %>% 
    filter(player_name == player) %>% 
    pull(pctFG)
  pctFG3 <- advanced %>% 
    filter(player_name == player) %>% 
    pull(pctFG3)
  player_stats <- tibble(
    team_abbr = team_abbr,
    position = position,
    gp = gp,
    mpg = mpg,
    ppg = ppg,
    rpg = rpg,
    apg = apg,
    pctFG = pctFG,
    pctFG3 = pctFG3
  )
  gt(player_stats) %>% 
    tab_header(paste(player)) %>% 
    cols_label(
      team_abbr = "Team",
      position = "Position",
      pctFG = "FG%",
      pctFG3 = "FG3%"
    ) %>% 
    fmt_percent(columns = starts_with("pct"), decimals = 1) %>% 
    gt_theme_538() %>% 
    opt_align_table_header("center") %>% 
    tab_options(
      table.background.color = "#f0f0f0",
      column_labels.background.color = "#f0f0f0"
    )
}
### Usage Plot
# Set up data
offense_overview <- advanced %>% 
  group_by(groupPosition) %>% 
  mutate(
    e_tov_pct = e_tov_pct / 100,
    usg_pct_percentile = percent_rank(usg_pct) * 100,
    ts_pct_percentile = percent_rank(ts_pct) * 100,
    ast_pct_percentile = percent_rank(ast_pct) * 100,
    e_tov_pct_percentile = percent_rank(e_tov_pct) * 100
  ) %>% 
  ungroup() %>% 
  select(player_name, short_player_name, groupPosition, usg_pct, usg_pct_percentile,
         ts_pct, ts_pct_percentile, ast_pct, ast_pct_percentile,
         e_tov_pct, e_tov_pct_percentile)
# Function
create_usage_plot <- function(player = player) {
  usage <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(usg_pct)
  usage_percentile <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(usg_pct_percentile)
  last_digit <- str_sub(as.character(round(usage_percentile, 0)), -1, -1)
  position <- offense_overview$groupPosition[which(offense_overview$player_name == player)]
  # Calculate density at brunson's usage
  density_data <- density(offense_overview$usg_pct[offense_overview$groupPosition == position])
  closest_point <- which.min(abs(density_data$x - usage))
  density_at_usage <- density_data$y[closest_point]
  usage_chart <- offense_overview %>% 
    filter(groupPosition == position) %>% 
    ggplot() +
    geom_density(aes(usg_pct), fill = "gray", alpha = 0.3) +
    geom_vline(xintercept = usage, color = "#006BB6", linewidth = 1) +
    geom_label(
      aes(
        x = if (usage_percentile < 90) {
          usage + .03
        } else {
          usage - .03
        },
        y = if (usage_percentile < 90) {
          density_at_usage + 1.5
        } else {
          usage + 5
        },
        label = if (usage_percentile > 10 & usage_percentile < 20) {
          paste0(player, " \n", usage * 100, " USG% \n", 
                 round(usage_percentile, 0), "th Percentile\nAmong ",
                 position, "s")
        } else if (last_digit == 1) {
          paste0(player, " \n", usage * 100, " USG% \n", 
                 round(usage_percentile, 0), "st Percentile\nAmong ",
                 position, "s")
        } else if (last_digit == 2) {
          paste0(player, " \n", usage * 100, " USG% \n", 
                 round(usage_percentile, 0), "nd Percentile\nAmong ",
                 position, "s")
        } else if (last_digit == 3) {
          paste0(player, " \n", usage * 100, " USG% \n", 
                 round(usage_percentile, 0), "rd Percentile\nAmong ",
                 position, "s")
        } else {
          paste0(player, " \n", usage * 100, " USG% \n", 
                 round(usage_percentile, 0), "th Percentile\nAmong ",
                 position, "s")
        }
      ),
      color = "#006BB6", fill = "gray"
    ) +
    xlim(c(0, 0.45)) + 
    scale_x_continuous(labels = scales::percent) +
    ylim(0, 12) +
  labs(
    title = paste0("Usage Breakdown - ", position, "s"),
    x = "",
    y = ""
  ) +
    theme_fivethirtyeight() +
    theme(
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(face = "italic"),
      text = element_text(family = "sans"),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank()
    )
  return(usage_chart)
}
### True Shooting Plot
create_ts_plot <- function(player = player) {
  position <- offense_overview$groupPosition[which(offense_overview$player_name == player)]
  usage_percentile <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(usg_pct_percentile)
  # all players within one standard deviation of usage
  ts_pct <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(ts_pct)
  point <- offense_overview %>% 
    filter(player_name == player)
  # players within 5% of usage percentile
  usage_percentile_threshold <- 5
  similar_usage_position <- offense_overview %>% 
    filter(groupPosition == position,
           usg_pct_percentile > usage_percentile - usage_percentile_threshold,
           usg_pct_percentile < usage_percentile + usage_percentile_threshold) %>% 
    mutate(e_tov_rank = rank(desc(ts_pct), ties.method = "min"))
  e_tov_rank <- similar_usage_position %>% 
    filter(player_name == player) %>% 
    pull(e_tov_rank)
  num_players <- nrow(similar_usage_position)
  ts_pct_chart <- offense_overview %>% 
    filter(groupPosition == position,
           usg_pct_percentile > usage_percentile - usage_percentile_threshold,
           usg_pct_percentile < usage_percentile + usage_percentile_threshold,
           player_name != player) %>% 
    ggplot() +
    geom_jitter(aes(ts_pct, y = 0.5), width = 0, height = 0.1, alpha = 0.3) +
    geom_point(data = offense_overview %>% filter(player_name == player), aes(ts_pct, y = 0.5), color = "#006BB6", size = 3) +
    geom_text_repel(data = similar_usage_position %>% filter(player_name != player),
                    aes(label = short_player_name, x = ts_pct, y = 0.5),
                    size = 3, color = "black", alpha = 0.5, force = 2) +
    geom_label_repel(
      data = point,
      aes(label = paste0(player, "\n", ts_pct * 100, " TS% \n",
                         "Ranks ", e_tov_rank, " of ", num_players,
                         " Similar-Usage ", position, "s"),
          x = ts_pct, y = 0.5),
      nudge_y = 0.5,
      color = "#006BB6",
      fill = "gray",
      size = 4
    ) +
    ylim(c(0, 1)) +
    labs(
      title = paste0("True Shooting Percentage of Similar-Usage ", position, "s"),
      caption = paste0("All ", position, "s within ", 
                       usage_percentile_threshold, "% of ", player, "'s usage percentile"),
      x = "",
      y = ""
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_fivethirtyeight() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 16),
          text = element_text(family = "sans")
    )
  ts_pct_chart
}

### Assist% Plot
create_assist_plot <- function(player = player) {
  position <- offense_overview$groupPosition[which(offense_overview$player_name == player)]
  usage_percentile <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(usg_pct_percentile)
  ast_pct <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(ast_pct)
  point <- offense_overview %>% 
    filter(player_name == player)
  usage_percentile_threshold <- 5
  similar_usage_position <- offense_overview %>% 
    filter(groupPosition == position,
           usg_pct_percentile > usage_percentile - usage_percentile_threshold,
           usg_pct_percentile < usage_percentile + usage_percentile_threshold) %>% 
    mutate(e_tov_rank = rank(desc(ast_pct), ties.method = "min"))
  e_tov_rank <- similar_usage_position %>% 
    filter(player_name == player) %>% 
    pull(e_tov_rank)
  num_players <- nrow(similar_usage_position)
  ast_pct_chart <- offense_overview %>% 
    filter(groupPosition == position,
           usg_pct_percentile > usage_percentile - usage_percentile_threshold,
           usg_pct_percentile < usage_percentile + usage_percentile_threshold,
           player_name != player) %>% 
    ggplot() +
    geom_jitter(aes(ast_pct, y = 0.5), width = 0, height = 0.1, alpha = 0.3) +
    geom_point(data = offense_overview %>% filter(player_name == player), aes(ast_pct, y = 0.5), color = "#006BB6", size = 3) +
    geom_text_repel(data = similar_usage_position %>% filter(player_name != player),
                    aes(label = short_player_name, x = ast_pct, y = 0.5),
                    size = 3, color = "black", alpha = 0.5, force = 2) +
    geom_label_repel(
      data = point,
      aes(label = paste0(player, "\n", ast_pct * 100, " AST% \n",
                         "Ranks ", e_tov_rank, " of ", num_players,
                         " Similar-Usage ", position, "s"),
          x = ast_pct, y = 0.5),
      nudge_y = 0.5,
      color = "#006BB6",
      fill = "gray",
      size = 4
    ) +
    ylim(c(0, 1)) +
    labs(
      title = paste0("Assist Percentage of Similar-Usage ", position, "s"),
      caption = paste0("All ", position, "s within ", 
                       usage_percentile_threshold, "% of ", player, "'s usage percentile"),
      x = "",
      y = ""
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_fivethirtyeight() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 16),
          text = element_text(family = "sans")
    )
  ast_pct_chart
}

### Turnover Plot
create_turnover_plot <- function(player = player) {
  position <- offense_overview$groupPosition[which(offense_overview$player_name == player)]
  usage_percentile <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(usg_pct_percentile)
  # all players within one standard deviation of usage
  e_tov_pct <- offense_overview %>% 
    filter(player_name == player) %>% 
    pull(e_tov_pct)
  point <- offense_overview %>% 
    filter(player_name == player)
  usage_percentile_threshold <- 5
  similar_usage_position <- offense_overview %>% 
    filter(groupPosition == position,
           usg_pct_percentile > usage_percentile - usage_percentile_threshold,
           usg_pct_percentile < usage_percentile + usage_percentile_threshold) %>% 
    mutate(e_tov_rank = rank(e_tov_pct, ties.method = "min"))
  e_tov_rank <- similar_usage_position %>% 
    filter(player_name == player) %>% 
    pull(e_tov_rank)
  num_players <- nrow(similar_usage_position)
  e_tov_pct_chart <- offense_overview %>% 
    filter(groupPosition == position,
           usg_pct_percentile > usage_percentile - usage_percentile_threshold,
           usg_pct_percentile < usage_percentile + usage_percentile_threshold,
           player_name != player) %>% 
    ggplot() +
    geom_jitter(aes(e_tov_pct, y = 0.5), width = 0, height = 0.1, alpha = 0.3) +
    geom_point(data = offense_overview %>% filter(player_name == player), aes(e_tov_pct, y = 0.5), color = "#006BB6", size = 3) +
    geom_text_repel(data = similar_usage_position %>% filter(player_name != player),
                    aes(label = short_player_name, x = e_tov_pct, y = 0.5),
                    size = 3, color = "black", alpha = 0.5) +
    geom_label_repel(
      data = point,
      aes(label = paste0(player, "\n", e_tov_pct * 100, " TO% \n",
                         "Ranks ", e_tov_rank, " of ", num_players,
                         " Similar-Usage ", position, "s"),
          x = e_tov_pct, y = 0.5),
      nudge_y = 0.5,
      color = "#006BB6",
      fill = "gray",
      size = 4
    ) +
    ylim(c(0, 1)) +
    labs(
      title = paste0("Turnover Percentage of Similar-Usage ", position, "s"),
      caption = paste0("All ", position, "s within ", 
                       usage_percentile_threshold, "% of ", player, "'s usage percentile"),
      x = "",
      y = ""
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_fivethirtyeight() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 16),
          text = element_text(family = "sans")
    )
  e_tov_pct_chart
}

#### Shooting Breakdown Page
### Shooting Table
## Prepare data
uncontested_twos <- closest_defender %>% 
  filter(mpg > 5, gp > 5, space %in% c("Open", "Wide open")) %>% 
  group_by(player_name, player_id, groupPosition) %>% 
  summarize(
    attempted = sum(fg2a),
    made = sum(fg2m),
  ) %>% 
  ungroup() %>% 
  mutate(
    percentage = made / attempted) %>% 
  group_by(groupPosition) %>% 
  mutate(
    percentile_percentage = percent_rank(percentage)) %>% 
  ungroup() %>% 
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>% 
  mutate(
    attempted_thirty_six = attempted / (mpg * countGames) * 36
  ) %>% 
  group_by(groupPosition.x) %>% 
  mutate(
    percentile_attempted_thirty_six = percent_rank(attempted_thirty_six)
  ) %>% 
  ungroup()
contested_twos <- closest_defender %>% 
  filter(mpg > 5, gp > 5, space %in% c("Very Tight", "Tight")) %>% 
  group_by(player_name, player_id, groupPosition) %>% 
  summarize(
    attempted = sum(fg2a),
    made = sum(fg2m),
  ) %>% 
  ungroup() %>% 
  mutate(
    percentage = made / attempted) %>% 
  group_by(groupPosition) %>% 
  mutate(
    percentile_percentage = percent_rank(percentage)) %>% 
  ungroup() %>% 
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>% 
  mutate(
    attempted_thirty_six = attempted / (mpg * countGames) * 36
  ) %>% 
  group_by(groupPosition.x) %>% 
  mutate(
    percentile_attempted_thirty_six = percent_rank(attempted_thirty_six)
  ) %>% 
  ungroup()
twos <- uncontested_twos %>% 
  left_join(contested_twos, by = c("player_name", "player_id", "groupPosition.x"),
            suffix = c("_open", "_contested")) %>% 
  mutate(shot_type = "TWO") %>% 
  relocate(shot_type, .after = groupPosition.x)
# Threes
uncontested_threes <- closest_defender %>% 
  filter(mpg > 5, gp > 5, space %in% c("Open", "Wide open")) %>% 
  group_by(player_name, player_id, groupPosition) %>% 
  summarize(
    attempted = sum(fg3a),
    made = sum(fg3m),
  ) %>% 
  ungroup() %>% 
  mutate(
    percentage = made / attempted) %>% 
  group_by(groupPosition) %>% 
  mutate(
    percentile_percentage = percent_rank(percentage)) %>% 
  ungroup() %>% 
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>% 
  mutate(
    attempted_thirty_six = attempted / (mpg * countGames) * 36
  ) %>% 
  group_by(groupPosition.x) %>% 
  mutate(
    percentile_attempted_thirty_six = percent_rank(attempted_thirty_six)
  ) %>% 
  ungroup()
contested_threes <- closest_defender %>% 
  filter(mpg > 5, gp > 5, space %in% c("Very Tight", "Tight")) %>% 
  group_by(player_name, player_id, groupPosition) %>% 
  summarize(
    attempted = sum(fg3a),
    made = sum(fg3m),
  ) %>% 
  ungroup() %>% 
  mutate(
    percentage = made / attempted) %>% 
  group_by(groupPosition) %>% 
  mutate(
    percentile_percentage = percent_rank(percentage)) %>% 
  ungroup() %>% 
  inner_join(player_positions, by = c("player_id" = "idPlayerNBA")) %>% 
  mutate(
    attempted_thirty_six = attempted / (mpg * countGames) * 36
  ) %>% 
  group_by(groupPosition.x) %>% 
  mutate(
    percentile_attempted_thirty_six = percent_rank(attempted_thirty_six)
  ) %>% 
  ungroup()
threes <- uncontested_threes %>% 
  left_join(contested_threes, by = c("player_name", "player_id", "groupPosition.x"),
            suffix = c("_open", "_contested")) %>% 
  mutate(shot_type = "THREE") %>% 
  relocate(shot_type, .after = groupPosition.x)
shooting_data <- rbind(twos, threes)
### Shooting Table
create_shooting_table <- function(player = player) {
  position <- advanced %>% 
    filter(player_name == player) %>% 
    pull(groupPosition)
  shooting_table <- shooting_data %>% 
    filter(player_name == player) %>% 
    select(shot_type,
           percentile_attempted_thirty_six_open,
           attempted_thirty_six_open,
           percentile_percentage_open,
           percentage_open,
           percentile_attempted_thirty_six_contested,
           attempted_thirty_six_contested,
           percentile_percentage_contested,
           percentage_contested
    ) %>% 
    mutate(across(contains("percentile"), ~.x * 100)) %>% 
    gt() %>% 
    tab_header(title = paste(player, "Shooting Breakdown")) %>% 
    tab_spanner("OPEN", ends_with("_open"), level = 2) %>% 
    tab_spanner("CONTESTED", ends_with("_contested"), level = 2) %>% 
    tab_spanner("FGA PER 36", c(percentile_attempted_thirty_six_open,
                                attempted_thirty_six_open), level = 1) %>% 
    tab_spanner("FG%", c(percentile_percentage_open,
                         percentage_open), level = 1) %>% 
    tab_spanner("FGA PER 36", c(percentile_attempted_thirty_six_contested,
                                attempted_thirty_six_contested), level = 1,
                id = 1) %>% 
    tab_spanner("FG%", c(percentile_percentage_contested,
                         percentage_contested), level = 1, id = 2) %>% 
    cols_label(
      shot_type = "",
      percentile_attempted_thirty_six_open = "",
      attempted_thirty_six_open = "",
      percentile_percentage_open = "",
      percentage_open = "",
      percentile_attempted_thirty_six_contested = "",
      attempted_thirty_six_contested = "",
      percentile_percentage_contested = "",
      percentage_contested = ""
    ) %>% 
    fmt_number(columns = contains("thirty_six"), decimals = 1) %>% 
    fmt_percent(columns = contains("percentage"), decimals = 1) %>% 
    fmt_number(columns = contains("percentile"), decimals = 0) %>% 
    # fmt_percent(columns = contains("percentile"), decimals = 0) %>% 
    data_color(
      columns = contains("percentile"),
      fn = scales::col_numeric(
        palette = c("#006BB6", "#f0f0f0", "orange"),
        domain = c(0, 100))
    ) %>% 
    tab_footnote(
      "Contested: Nearest Defender < 4 Feet Away"
    ) %>% 
    tab_footnote(paste0("Percentiles Relative to Other ", position, "s")) %>% 
    gt_theme_538() %>% 
    opt_align_table_header("center") %>% 
    tab_options(
      table.background.color = "#f0f0f0",
      column_labels.background.color = "#f0f0f0"
    )
  return(shooting_table)
}
### Shot Chart
# tutorial: https://github.com/DomSamangy/R_Tutorials/blob/main/3_Heatmap_Tutorial.Rmd
## Setup Functions -- Don't require input
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

# Court Dimensions & lines
width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

# Court themes
court_themes = list(
  light = list(
    court = "blue",
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#f0f0f0"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray15',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray15"
  )
)

# Function to create court based on given dimensions
plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  temp <- data.frame(x = -30, 
                     xmax =30,
                     y = -5, ymax = 50)
  # Final plot creation
  ggplot() +
    geom_rect(data = temp,aes(xmin = x,
                              ymin = y,
                              ymax = ymax,
                              xmax = xmax), fill = '#f0f0f0') +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = '#f0f0f0', color = '#f0f0f0'),
      panel.background = element_rect(fill = "#f0f0f0", color = '#f0f0f0'),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}
## Function to create the court
create_shooting_location_plot <- function(player = player) {
  player_shot_data <- team_shot_data %>% filter(namePlayer == player) %>% 
    mutate(x = as.numeric(as.character(locationX)) / 10, y = as.numeric(as.character(locationY)) / 10 + hoop_center_y)
  # Horizontally flip the data
  player_shot_data$x <- player_shot_data$x * -1 
  palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = -1)
  shot_chart <- plot_court(court_themes$light) + 
    geom_density_2d_filled(player_shot_data, mapping = aes(x=x,y=y,fill = ..level..,), 
                           # adjust to change threshold of heat map 
                           contour_var = "ndensity", breaks = c(seq(0.05, 0.2, length.out = 5), seq(0.3, 1, length.out = 4)), alpha = .5)  + 
    scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) +
    scale_x_continuous(limits = c(-25, 25)) + 
    scale_y_continuous(limits = c(0, 45)) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = '#f0f0f0', color = '#f0f0f0'),
          panel.background = element_rect(fill = "#f0f0f0", color = '#f0f0f0'),
          plot.title = element_text(hjust = .5, size = 22, face = "bold", vjust = -4,
                                    family = "sans"),
          plot.subtitle = element_text(hjust = .5, size = 10, face = "bold", vjust = -8),
          legend.title = element_blank(),
          legend.text = element_text(hjust = .5, size = 10, face = "bold", colour = "white"),
          plot.caption = element_text(hjust = .5, size = 10, face = "bold", colour = "darkgrey", vjust = 8)) +
    labs(title = paste(player, "Shot Heatmap"),
         caption = "Code from Dom Samangy")
  return(shot_chart)
}
### Play Type Plot
# Prepare data
play_type_ratings <- play_type_data %>% 
  group_by(player_name, player_id, play_type, groupPosition) %>% 
  summarize(
    fga_thirty_six = mean(fga_thirty_six),
    efficiency = mean(ppp)
  )
# Calculate percentiles
percentiles <- play_type_ratings %>% 
  group_by(play_type, groupPosition) %>% 
  mutate(
    frequency_percentile = percent_rank(fga_thirty_six) * 100,
    efficiency_percentile = percent_rank(efficiency) * 100
  )
# Function
create_play_type_plot <- function(player = player) {
  play_type_data <- percentiles %>% 
    filter(player_name == player,
           frequency_percentile > 1) %>% 
    select(play_type, frequency_percentile, efficiency_percentile)
  position <- advanced %>% 
    filter(player_name == player) %>% 
    pull(groupPosition)
  play_type_data$play_type <- factor(play_type_data$play_type, levels = play_type_data$play_type[order(play_type_data$frequency_percentile)])
  
  play_type_chart <- ggplot(data = play_type_data, aes(y = play_type)) +
    geom_segment(aes(x = frequency_percentile, xend = efficiency_percentile, 
                     y = play_type, yend = play_type), color = "gray") +
    geom_point(aes(x = frequency_percentile, color = "Frequency"), size = 5) +
    geom_point(aes(x = efficiency_percentile, color = "Efficiency"), size = 5) +
    scale_color_manual(values = c("Frequency" = "#006BB6", "Efficiency" = "#F58426")) +
    xlim(c(0, 100)) +
    labs(title = paste0(player, ": <br> Play Type <span style='color:#006BB6;'>Frequency</span> and <span style='color:#F58426;'>Efficiency</span>"),
         x = "Percentile",
         y = "Play Type",
         color = "Metric",
         caption = paste0("Frequency: FGA per 36 \n
         Efficiency: Points per Possession \n
         Percentiles Relative to Other ", position, "s")) +
    theme_fivethirtyeight() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "None",
          plot.title = element_markdown(),
          axis.text.y = element_text(),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          plot.caption = element_text())
  return(play_type_chart)
}
#### Defense and Rebounding Page
### Hustle Stats Table
# Prepare Data
defense_table_data <- hustle %>%
  inner_join(boxout, by = "player_id") %>% 
  filter(
    countGames >= 10,
    mpg >= 5
  ) %>% 
  # make all stats per 36
  mutate(
    deflections = round((deflections / mpg) * 36, 1),
    loose_balls_recovered = round((loose_balls_recovered / mpg) * 36, 1),
    charges_drawn = round((charges_drawn / mpg) * 36, 1),
    contested_shots = round((contested_shots / mpg) * 36, 1),
    off_boxouts = round((off_boxouts / mpg) * 36, 1),
    def_boxouts = round((def_boxouts / mpg) * 36, 1),
  ) %>% 
  group_by(groupPosition) %>% 
  mutate(
    deflections_percentile = percent_rank(deflections) * 100,
    loose_balls_recovered_percentile = percent_rank(loose_balls_recovered) * 100,
    charges_drawn_percentile = percent_rank(charges_drawn) * 100,
    contested_shots_percentile = percent_rank(contested_shots) * 100,
    off_boxouts_percentile = percent_rank(off_boxouts) * 100,
    def_boxouts_percentile = percent_rank(def_boxouts) * 100,
  ) %>% 
  ungroup() %>% 
  select(
    player_name, groupPosition,
    deflections_percentile, deflections,
    loose_balls_recovered_percentile, loose_balls_recovered,
    charges_drawn_percentile, charges_drawn,
    contested_shots_percentile, contested_shots,
    off_boxouts_percentile, off_boxouts,
    def_boxouts_percentile, def_boxouts
  )
# Function
create_hustle_table <- function(player=player) {
  position <- defense_table_data$groupPosition[which(defense_table_data$player_name == player)]
  def_columns <- c(
    "deflections_percentile", "deflections",
    "loose_balls_recovered_percentile", "loose_balls_recovered",
    "charges_drawn_percentile", "charges_drawn",
    "contested_shots_percentile", "contested_shots"
  )
  rebounding_columns <- c(
    "off_boxouts_percentile", "off_boxouts",
    "def_boxouts_percentile", "def_boxouts"
  )
  hustle_table <- defense_table_data %>% 
    filter(player_name == player) %>%
    select(-c(player_name, groupPosition)) %>% 
    gt() %>% 
    tab_header(paste(player, "Hustle Stats")) %>% 
    tab_spanner("DEFENSE", columns = def_columns, level = 2) %>% 
    tab_spanner("REBOUNDING", columns = rebounding_columns, level = 2) %>% 
    tab_spanner("DEFLECTIONS", columns = contains("deflections"), level = 1) %>% 
    tab_spanner("LOOSE BALLS RECOVERED", columns = contains("loose_balls"), level = 1) %>%
    tab_spanner("CHARGES DRAWN", columns = contains("charges_drawn"), level = 1) %>%
    tab_spanner("SHOT CONTESTS", columns = contains("contested_shots"), level = 1) %>%
    tab_spanner("OFF BOXOUTS", columns = contains("off_boxouts"), level = 1) %>%
    tab_spanner("DEF BOXOUTS", columns = contains("def_boxouts"), level = 1) %>%
    cols_label(
      deflections = "",
      contains("_") ~ ""
    ) %>% 
    fmt_number(columns = contains("percentile"), decimals = 0) %>% 
    tab_footnote(paste0("Percentiles Relative to Other ", position, "s")) %>% 
    tab_footnote("All Stats Per 36 Minutes") %>% 
    data_color(
      columns = contains("percentile"),
      fn = scales::col_numeric(
        palette = c("#006BB6", "#f0f0f0", "orange"),
        domain = c(0, 100))
    ) %>%
    gt_theme_538() %>% 
    opt_align_table_header("center") %>% 
    tab_options(
      table.background.color = "#f0f0f0",
      column_labels.background.color = "#f0f0f0"
    )
  return(hustle_table)
}
### Defense Scatter Plot
create_defense_plot <- function(player = player) {
  position <- defense$groupPosition[which(defense$player_name == player)]
  defense_rebounding_point <- advanced %>% 
    filter(player_name == player)
  steal_block_rate <- defense %>% 
    mutate(steals_thirty_six = (stl / mpg) * 36,
           blocks_thirty_six = (blk / mpg) * 36) %>% 
    group_by(groupPosition) %>% 
    mutate(
      steal_rate_percentile = percent_rank(steals_thirty_six) * 100,
      block_rate_percentile = percent_rank(blocks_thirty_six) * 100
    )
  steal_rate <- steal_block_rate %>% 
    filter(player_name == player) %>% 
    pull(steals_thirty_six)
  steal_percentile <- steal_block_rate %>% 
    filter(player_name == player) %>% 
    pull(steal_rate_percentile)
  block_rate <- steal_block_rate %>% 
    filter(player_name == player) %>% 
    pull(blocks_thirty_six)
  block_percentile <- steal_block_rate %>% 
    filter(player_name == player) %>% 
    pull(block_rate_percentile)
  steal_block_rate_averages <- steal_block_rate %>% 
    filter(groupPosition == position) %>% 
    summarize(mean_stl = mean(steals_thirty_six),
              mean_blk = mean(blocks_thirty_six))
  defense_chart <- steal_block_rate %>% 
    filter(groupPosition == position) %>%
    ggplot() +
    geom_point(aes(steals_thirty_six, blocks_thirty_six), alpha = 0.1) +
    geom_point(data = steal_block_rate %>% filter(player_name == player),
               aes(steals_thirty_six, blocks_thirty_six),
               size = 4, alpha = 1, fill = "#006BB6", color = "#006BB6",
               shape = 21) +
    geom_hline(yintercept = steal_block_rate_averages$mean_blk, linetype = "dashed",
               alpha = 0.5) +
    geom_vline(xintercept = steal_block_rate_averages$mean_stl, linetype = "dashed",
               alpha = 0.5) +
    labs(
      title = paste0(player, " Defense Relative to \n Other ", position, "s"),
      x = "Steals / 36",
      y = "Blocks / 36"
    ) +
    geom_label_repel(
      data = defense_rebounding_point,
      label = paste0("Steals/36: ", round(steal_rate, 1),
                     " (Percentile: ", round(steal_percentile, 0), ") \n",
                     "Blocks/36: ", round(block_rate, 1),
                     " (Percentile: ", round(block_percentile, 0), ")"
      ),
      x = steal_rate,
      y = block_rate,
      color = "#006BB6",
      fill = "gray"
    ) + 
    theme_fivethirtyeight() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = 'bold', size = 18),
      plot.title.position = 'plot',
      plot.subtitle = element_text(size = 8),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    ) 
  return(defense_chart)
}
### Rebounding Scatter Plot
create_rebounding_plot <- function(player = player) {
  position <- defense$groupPosition[which(defense$player_name == player)]
  defense_rebounding_point <- advanced %>% 
    filter(player_name == player)
  rebounding_percentiles <- advanced %>% 
    group_by(groupPosition) %>% 
    mutate(
      oreb_percentile = percent_rank(oreb_pct) * 100,
      dreb_percentile = percent_rank(dreb_pct) * 100) %>% 
    select(player_name,
           groupPosition,
           oreb_pct,
           oreb_percentile,
           dreb_pct,
           dreb_percentile)
  oreb <- advanced %>% 
    filter(player_name == player) %>% 
    pull(oreb_pct)
  oreb_percentile <- rebounding_percentiles %>% 
    filter(player_name == player) %>% 
    pull(oreb_percentile)
  dreb <- advanced %>% 
    filter(player_name == player) %>% 
    pull(dreb_pct)
  dreb_percentile <- rebounding_percentiles %>% 
    filter(player_name == player) %>% 
    pull(dreb_percentile)
  reb_average <- advanced %>% 
    filter(groupPosition == position,
           mpg > 5) %>% 
    summarize(mean_oreb_pct = mean(oreb_pct),
              mean_dreb_pct = mean(dreb_pct))
  rebounding_chart <- advanced %>% 
    filter(groupPosition == position,
           mpg > 5
    ) %>%
    ggplot() +
    geom_point(aes(oreb_pct, dreb_pct), alpha = 0.1) +
    geom_point(data = advanced %>% filter(player_name == player),
               aes(oreb_pct, dreb_pct),
               size = 4, alpha = 1, fill = "#006BB6", color = "#006BB6",
               shape = 21) +
    geom_hline(yintercept = reb_average$mean_dreb_pct, linetype = "dashed",
               alpha = 0.5) +
    geom_vline(xintercept = reb_average$mean_oreb_pct, linetype = "dashed",
               alpha = 0.5) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste0(player, " Rebounding Relative to \n Other ", position, "s"),
      x = "OReb%",
      y = "DReb%"
    ) +
    geom_label_repel(
      data = defense_rebounding_point,
      label = paste0("OReb%: ", oreb*100,
                     "% (Percentile: ", round(oreb_percentile, 0), ") \n",
                     "DReb%: ", dreb*100,
                     "% (Percentile: ", round(dreb_percentile, 0), ")"
      ),
      x = oreb,
      y = dreb,
      color = "#006BB6",
      fill = "gray"
    ) + 
    theme_fivethirtyeight() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = 'bold', size = 18),
      plot.title.position = 'plot',
      plot.subtitle = element_text(size = 8),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
    ) 
  return(rebounding_chart)
}
#### Functions that produce plots / tables:
# Argument for every function is just player
## Page 1: offensive summary
# create_usage_plot: usage density plot
# create_ts_plot: true shooting scatter plot
# create_assist_plot: assist% scatter plot
# create_usage_plot: turnover% shooting scatter plot
## Page 2: offensive breakdown
# create_shooting_table: twos/threes shooting breakdown
# create_shooting_location_plot: shot heat map
# create_play_type_plot: play type bar chart
## Page 3: defense and rebounding
# create_hustle_table: hustle stats table
# create_defense_plot: steals and blocks scatter plot
# create_rebounding_plot: rebounding% scatter plot

################################################################################
##### Shiny App
# List of players for dropdown menu
players <- unique(offense_overview$player_name)
# Headshots for sidebar
player_ids <- unique(player_positions$idPlayerNBA)
headshot_urls <- nba_playerheadshot(player_ids)
headshot_urls <- lapply(player_ids, nba_playerheadshot)
headshot_urls <- unlist(headshot_urls)
player_headshots_df <- tibble(player_id = player_ids,
                              headshot_url = headshot_urls) %>% 
  inner_join(advanced %>% select(player_id, player_name), by = "player_id")
# User interface specifications
ui <- dashboardPage(
  # Specify header
  header = dashboardHeader(title = "NBA Player Analytics"),
  # Sidebar -- allow users to navigate between pages
  sidebar = dashboardSidebar(
    sidebarMenu(id = "sidebarMenuID",
                ## Pages to navigate
                # Offensive Overview
                menuItem("Offensive Overview",
                         tabName = "overview"),
                # Shooting Breakdown
                menuItem("Shooting Breakdown",
                         tabName = "shooting_breakdown"),
                # Defensive Breakdown
                menuItem("Defense and Rebounding",
                         tabName = "defense_rebounding")
    ),
    selectizeInput('player_name', 'Select Player:', players,
                   options = list(placeholder = "Type to search...")),
    uiOutput("player_headshot")
  ),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .main-footer {
        background-color: #f0f0f0 !important;
      }
      .main-sidebar, .left-side {
        background-color: #006BB6 !important;
      }
      .main-header .logo, .main-header .navbar {
        background-color: #006BB6 !important;
      }"
      )
      )
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                column(6,
                       div(style = "height: 100px;"),
                       gt_output('summary_table'),
                       div(style = "height: 50px;"),
                       plotOutput('usage_plot', height = "400px"),
                       div(style = "height: 100px;")
                ),
                column(6,
                       plotOutput('true_shooting_plot', height = "250px"),
                       plotOutput('assist_plot', height = "250px"),
                       plotOutput('turnover_plot', height = '250px')
                )
              )
      ),
      tabItem(tabName = "shooting_breakdown",
              fluidRow(
                column(12, gt_output('shooting_table'))
              ),
              fluidRow(
                #column(0.5),
                # CHANGE height and column widths
                column(6, plotOutput('shot_chart')),
                # column(1),
                column(6, plotOutput('play_type_plot'))
              ),
      ),
      tabItem(tabName = "defense_rebounding",
              fluidRow(
                column(12, gt_output('hustle_table')),
              ),
              fluidRow(
                column(6, plotOutput('defense_plot')),
                column(6, plotOutput('rebounding_plot'))
              )
      )
    )
  )
)
# Function for the inputs and outputs
server <- function(input, output, session) {
  # Reactive to allow user to input player name
  player  <- reactive({input$player_name})
  # player headshots
  output$player_headshot <- renderUI({
    player <- input$player_name
    headshot_url <- player_headshots_df$headshot_url[player_headshots_df$player_name == player]
    
    if(!is.null(headshot_url) && headshot_url != "") {
      tags$div(
        tags$img(src = headshot_url, alt = "Player headshot", style = "display: block; margin-left: auto; margin-right: auto; height: 140px; width: 200px;"),
        style = "text-align: center;"
      )
    } else {
      "Headshot not available"
    }
  })
  ## Plot outputs -- vary with user input
  # Offensive Overview
  output$summary_table <- render_gt({
    create_summary_table(player())
  })
  output$usage_plot <- renderPlot({
    create_usage_plot(player())
  })
  output$true_shooting_plot <- renderPlot({
    create_ts_plot(player())
  })
  output$assist_plot <- renderPlot({
    create_assist_plot(player())
  })
  output$turnover_plot <- renderPlot({
    create_turnover_plot(player())
  })
  # Shooting Breakdown
  output$shooting_table <- render_gt({
    create_shooting_table(player())
  })
  output$shot_chart <- renderPlot({
    create_shooting_location_plot(player())
  })
  output$play_type_plot <- renderPlot({
    create_play_type_plot(player())
  })
  # Defense and Rebounding
  output$hustle_table <- render_gt({
    create_hustle_table(player())
  })
  output$defense_plot <- renderPlot({
    create_defense_plot(player())
  })
  output$rebounding_plot <- renderPlot({
    create_rebounding_plot(player())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
