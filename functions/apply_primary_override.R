# apply_primary_override.R — override betless predictions with primary-model ones.
#
# Pure (no Keras): given the betless-predicted future_data_lean and the primary
# model's predictions for the upcoming matches that have odds, replace the betless
# pred_loss_prob / pred_win_prob for those rows only (results == 999 with a
# non-NA primary prediction). Rows without a primary prediction keep the betless
# fallback. Used by predict_round_core; unit-tested in tests/check_primary_override.R.
#
# primary_preds: one row per overridable match with key columns
#   team, opposition, Season, status and pred_loss_prob_primary, pred_win_prob_primary.
apply_primary_override <- function(future_data_lean, primary_preds) {
  future_data_lean %>%
    left_join(primary_preds, by = c("team", "opposition", "Season", "status")) %>%
    mutate(
      pred_loss_prob = ifelse(!is.na(pred_loss_prob_primary) & results == 999,
                              pred_loss_prob_primary, pred_loss_prob),
      pred_win_prob  = ifelse(!is.na(pred_win_prob_primary) & results == 999,
                              pred_win_prob_primary, pred_win_prob)
    ) %>%
    select(-pred_loss_prob_primary, -pred_win_prob_primary)
}
