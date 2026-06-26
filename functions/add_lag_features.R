# add_lag_features.R — add lagged ("last_*") features in one grouped pass.
#
# For each `new = source` pair in `lags`, adds a column
#   new = lag(source, order_by = <order>)
# within group_by(<group>). Replaces the repeated
#   group_by(Team) %>% mutate(last_x = lag(x, order_by = Date), ...)
# blocks in build_features_core.
#
# All lags are applied in a single mutate, in `lags` order, so a later lag may
# reference an earlier one (e.g. last_oppRate = lag(pre_oppRate)) — dplyr's
# sequential mutate + the .data pronoun make the just-created column visible.
#
#   group — character vector of grouping columns
#   lags  — named character vector: new_column = source_column
#   order — column to order_by within each group (default "Date")
add_lag_features <- function(df, group, lags, order = "Date") {
  exprs <- stats::setNames(
    lapply(lags, function(src) {
      rlang::expr(dplyr::lag(.data[[!!src]], order_by = .data[[!!order]]))
    }),
    names(lags)
  )
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) %>%
    dplyr::mutate(!!!exprs) %>%
    dplyr::ungroup()
}
