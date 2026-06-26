library(ggpubr)
library(reactable)
library(htmltools)
library(sparkline)
library(dplyr)      # declare own deps rather than rely on upstream tidyverse
library(magrittr)

source('functions/reactable_function.R')

# =============================================================================
# Stage 4 — archive predictions + render the selections table.
#
# Core assembles the round's prediction rows (t) and team-rating rows
# (new_ratings) from the upstream frames — pure, golden-tested. The shell merges
# those into the archive CSVs (read existing, drop this round's entry, append,
# write) and renders the reactable. season is parameterised (defaults to the
# current year) so the core is deterministic for tests.
# =============================================================================

# ---- core: upstream frames -> rows to archive + display ---------------------
assemble_round_outputs <- function(future_data_lean, score_data_lean, round, glicko_rate, round.no,
                                    season = as.numeric(format(Sys.Date(), "%Y"))) {
  #bind guess with fixture
  new_predictions <- future_data_lean %>%
    left_join(
      score_data_lean %>%
        select(Team, Opposition, Margin, Season, team, opposition, status, venue, matchType)
    ) %>%
    filter(Margin == 999) %>%
    mutate(Tips = ifelse(pred_win_prob > 0.5, 1, 0)) %>%
    select(Team, Opposition, Tips, pred_loss_prob, pred_win_prob, margin_est_linear)

  table <- round %>%
    left_join(new_predictions, by = c("Team", "Opposition"))

  table$Margin <- NULL

  table %<>%
    mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition))

  table %<>%
    select(Date, Match_id, Season, Team, Opposition, Status, Venue, Round, results, Odds, line_Odds, Opp_Odds, Opp_lineOdds, Tips, pred_loss_prob, pred_win_prob, Team_predicted, margin_est_linear) %>%
    rename(
      Loss_prob = pred_loss_prob,
      Win_Prob = pred_win_prob,
      margin_estimate_1 = margin_est_linear
    )

  table_final <- table %>%
    filter(Status == "Home")

  table_final %<>%
    select(Date, Season, Team, Opposition, Venue, Round, Loss_prob, Win_Prob, Team_predicted, margin_estimate_1) %>%
    mutate(Loss_prob = round(Loss_prob, digits = 2)) %>%
    mutate(Win_Prob = round(Win_Prob, digits = 2))

  # generate table to merge with simulation plot
  t <- table_final %>%
    select(Team, Opposition, Round, Loss_prob, Win_Prob, margin_estimate_1, Team_predicted) %>%
    rename(Pred_Winner=Team_predicted, Pred_Margin = margin_estimate_1) %>%
    mutate(Pred_Margin = round(Pred_Margin,0),
           Pred_Margin = ifelse(Win_Prob >=0.5 & Pred_Margin < 0, 0,
                                ifelse(Win_Prob <= 0.5 & Pred_Margin > 0, 0, Pred_Margin)),
           Season = season)

  current_round <- unique(t$Round)
  current_season <- as.integer(unique(t$Season))

  # Team rating rows — appended to history under round.no - 1
  new_ratings <- data.frame(
    Season = current_season,
    Team   = glicko_rate$ratings$Player,
    Round  = round.no - 1,
    rating = glicko_rate$ratings$Rating
  )

  list(t = t, new_ratings = new_ratings,
       current_round = current_round, current_season = current_season)
}

# ---- IO shell: merge into the archives, render the table --------------------
store_round <- function(future_data_lean, score_data_lean, round, glicko_rate, round.no) {
  out <- assemble_round_outputs(future_data_lean, score_data_lean, round, glicko_rate, round.no)
  t              <- out$t
  current_round  <- out$current_round
  current_season <- out$current_season

  # round predictions archive — drop this round's entry, append the new rows
  season_pred <- read.csv('csv_files/round_predictions.csv')
  season_pred <- season_pred[!(season_pred$Season == current_season & season_pred$Round == current_round),
                             c("Team","Opposition","Round","Loss_prob","Win_Prob","Pred_Margin","Pred_Winner", "Season")]
  season_pred.new <- rbind(season_pred, t)
  write.csv(season_pred.new, 'csv_files/round_predictions.csv')

  # team ratings archive — drop this round's entry, append
  existing_ratings <- read.csv('csv_files/Team_Ratings.csv')
  existing_ratings <- existing_ratings[!(existing_ratings$Season == current_season &
                                           existing_ratings$Round == round.no - 1), ]
  rbind(existing_ratings, out$new_ratings) %>%
    write.csv('csv_files/Team_Ratings.csv', row.names = FALSE)

  # use reactable to use team logos
  options(reactable.theme = reactableTheme(
    color = "hsl(233, 9%, 87%)",
    backgroundColor = "hsl(233, 9%, 19%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  ))

  predictions_table <- reactable(t, columns = list(
    Team = colDef(maxWidth = 150, align = "center", cell = function(value) {
      img_src <- knitr::image_uri(sprintf("images/%s.png", value))
      image <- img(src = img_src, height = "60px", alt = value)
      tagList(
        div(style = list(display = "inline-block", width = "80px"), image)
      )

    }),
    Opposition = colDef(maxWidth = 150, align = "center", cell = function(value) {
      img_src <- knitr::image_uri(sprintf("images/%s.png", value))
      image <- img(src = img_src, height = "60px", alt = value)
      tagList(
        div(style = list(display = "inline-block", width = "80px"), image)
      )
    }),
    Pred_Winner = colDef(name = "Predicted Winner", maxWidth = 150, align = "center", cell = function(value) {
      img_src <- knitr::image_uri(sprintf("images/%s.png", value))
      image <- img(src = img_src, height = "60px", alt = value)
      tagList(
        div(style = list(display = "inline-block", width = "80px"), image)
      )
    }),
    Round = colDef(align = "center", maxWidth = 120),
    Loss_prob = colDef(name = "Loss Probability", align = "center", maxWidth = 120),
    Win_Prob = colDef(name = "Win Probability", align = "center", maxWidth = 120),
    Pred_Margin = colDef(name = "Predicted Margin", align = "center", maxWidth = 120, format = colFormat(digits = 0))
  ))

  print(htmltools::browsable(predictions_table))

  list(predictions_table = predictions_table,
       current_round = current_round, current_season = current_season)
}
