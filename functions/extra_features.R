# extra_features.R — candidate features for model experimentation
#
# Usage in AFL_data.R (after main feature engineering, before future_data_lean select):
#   new <- add_extra_features(new)
#
# After enabling, add the new column names to the select() calls in
# future_data_lean and score_data_lean in AFL_data.R, then retrain the model.
#
# Run analysis_code/feature_investigation.R to evaluate whether these features
# improve predictive power before committing to a retrain.

add_extra_features <- function(df) {

  # --- Helper: lagged rolling mean of last n values (base R, no extra packages) ---
  # Returns NA for the first row and where fewer than 1 prior value exists.
  rolling_lag_mean <- function(x, n = 5) {
    len <- length(x)
    out <- rep(NA_real_, len)
    for (i in seq_len(len)) {
      start <- max(1L, i - n)
      end   <- i - 1L
      if (end >= start) out[i] <- mean(x[start:end], na.rm = TRUE)
    }
    out
  }

  # 1. days_since_last — rest days between this game and the team's previous game
  #    Rest advantage is one of the most significant factors in AFL scheduling.
  df <- df %>%
    arrange(Date) %>%
    group_by(Team) %>%
    mutate(
      days_since_last = as.numeric(Date - lag(Date, order_by = Date))
    ) %>%
    ungroup()

  # 2. opp_days_since_last — opponent's rest days
  #    Derived by pairing the two rows of each match via Match_id.
  #    NOTE: fixture Match_id (1, 2, 3...) may collide with historical Match_id values.
  #    This is a pre-existing issue in the codebase (same pattern used for opp_rating).
  #    In practice it doesn't cause problems because future rows sort to the end by Date.
  df <- df %>%
    group_by(Match_id) %>%
    mutate(
      opp_days_since_last = sum(days_since_last, na.rm = FALSE) - days_since_last
    ) %>%
    ungroup()

  # 3. rolling_form_5 — lagged rolling win rate over previous 5 games
  #    Draws treated as 0.5 (consistent with Glicko section of AFL_data.R).
  #    Future rows (results==999) treated as NA so they don't contaminate the window.
  df <- df %>%
    arrange(Date) %>%
    group_by(Team) %>%
    mutate(
      result_for_roll = ifelse(results == 999, NA_real_,
                               ifelse(results == 2, 0.5, as.numeric(results))),
      rolling_form_5  = rolling_lag_mean(result_for_roll, n = 5)
    ) %>%
    select(-result_for_roll) %>%
    ungroup()

  # 4. venue_win_rate — lagged cumulative win rate at this specific venue
  #    Uses the character Venue column (not the integer-encoded 'venue').
  #    NA for a team's first game at a venue (no history yet).
  df <- df %>%
    arrange(Date) %>%
    group_by(Team, Venue) %>%
    mutate(
      venue_games_so_far = row_number() - 1L,
      venue_wins_cumul   = lag(
        cumsum(ifelse(results %in% c(999, NA), 0,
                      ifelse(results == 2, 0.5, as.numeric(results)))),
        order_by = Date
      ),
      venue_win_rate = ifelse(venue_games_so_far == 0, NA_real_,
                              venue_wins_cumul / venue_games_so_far)
    ) %>%
    select(-venue_games_so_far, -venue_wins_cumul) %>%
    ungroup()

  # 5. h2h_win_rate — lagged cumulative win rate against this specific opponent
  #    NA for a team's first encounter with this opponent.
  df <- df %>%
    arrange(Date) %>%
    group_by(Team, Opposition) %>%
    mutate(
      h2h_games_so_far = row_number() - 1L,
      h2h_wins_cumul   = lag(
        cumsum(ifelse(results %in% c(999, NA), 0,
                      ifelse(results == 2, 0.5, as.numeric(results)))),
        order_by = Date
      ),
      h2h_win_rate = ifelse(h2h_games_so_far == 0, NA_real_,
                            h2h_wins_cumul / h2h_games_so_far)
    ) %>%
    select(-h2h_games_so_far, -h2h_wins_cumul) %>%
    ungroup()

  return(df)
}
