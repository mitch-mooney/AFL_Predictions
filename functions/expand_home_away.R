# expand_home_away.R — reshape one-row-per-match into one-row-per-team.
#
# Collapses the row-duplication + Home/Away convention that was copy-pasted across
# the results, betting, and fixture reshapes. Duplicates each match into two rows,
# adds num (1 = Home, 2 = Away), Status, Team, Opposition, and swaps any paired
# Home/Away value columns.
#
# Args:
#   df    — one row per match, with `home`/`away` team columns.
#   key   — character vector of columns that uniquely identify a match (its two
#           duplicated rows share them); used to number Home/Away within a match.
#   pairs — named list mapping a new column to c(home_value_col, away_value_col).
#           num == 1 (Home) takes the first element, num == 2 (Away) the second.
#   home, away — the team column names (default Home.Team / Away.Team).
expand_home_away <- function(df, key, pairs = list(),
                             home = "Home.Team", away = "Away.Team") {
  out <- df[rep(1:nrow(df), 2), , drop = FALSE] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) %>%
    dplyr::mutate(
      num        = dplyr::row_number(),
      Status     = ifelse(num == 1, "Home", "Away"),
      Team       = ifelse(num == 1, .data[[home]], .data[[away]]),
      Opposition = ifelse(num == 1, .data[[away]], .data[[home]])
    ) %>%
    dplyr::ungroup()

  for (nm in names(pairs)) {
    p <- pairs[[nm]]
    out[[nm]] <- ifelse(out$num == 1, out[[p[1]]], out[[p[2]]])
  }
  out
}
