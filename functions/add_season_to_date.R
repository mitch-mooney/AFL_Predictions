# add_season_to_date.R — season-to-date totals BEFORE the current match.
#
# For each `new = source` pair, adds
#   new = lag(cumsum(source), 1, default = 0)
# within group_by(<group>) — the running total over the season EXCLUDING the
# current match (0 for a team's first game of the season). Used for season_for /
# season_against in build_features_core.
#
# Source columns may be NA on upcoming (unplayed) rows; because the value is
# lagged, that NA only lands in the discarded current-row cumsum.
#
#   group — grouping columns (default c("Season", "Team"))
#   cols  — named character vector: new_column = source_column
add_season_to_date <- function(df, cols, group = c("Season", "Team")) {
  exprs <- stats::setNames(
    lapply(cols, function(src) {
      rlang::expr(dplyr::lag(cumsum(.data[[!!src]]), 1, default = 0))
    }),
    names(cols)
  )
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>%
    dplyr::mutate(!!!exprs) %>%
    dplyr::ungroup()
}
