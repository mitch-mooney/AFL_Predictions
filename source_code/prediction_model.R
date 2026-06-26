library(keras)
library(dplyr)     # declare own deps rather than rely on betting_odds.R's tidyverse
library(magrittr)

# =============================================================================
# Stage 3 — win-probability + margin predictions.
#
# Split into a core and a thin IO shell (mirrors build_features). The shell loads
# the Keras models (the only disk IO); the core runs model_data + inference with
# the injected models and assembles the outputs. model_data() and the Keras ops
# (predict / k_argmax / to_categorical) keep the core Keras-coupled, so it is
# guarded by a golden test against the committed, deterministic model files rather
# than a hermetic stub.
#
# The pure primary/betless override is its own helper, functions/apply_primary_override.R
# (unit-tested in tests/check_primary_override.R).
# =============================================================================

# ---- core: feature frames + loaded models -> predictions --------------------
# Returns list(prob_pred_df, future_data_lean) — future_data_lean enriched with
# pred_loss_prob, pred_win_prob, pred_cat, margin_est_linear.
predict_round_core <- function(future_data_lean, score_data_lean,
                               future_data_full, model, model_primary = NULL) {

  # Betless model always runs first — baseline/fallback for all matches.
  # matchType is dropped to match how the betless model is trained in retrain_model.R.
  data <- future_data_lean %>% select(-matchType)
  col_num <- as.numeric(ncol(data))
  data[1:col_num] <- lapply(data[1:col_num], as.numeric)

  model.data <- model_data(data)

  # evaluate + confusion table (diagnostic only)
  model %>% evaluate(model.data$test, model.data$testLabels)
  pred <- model %>% predict(model.data$test) %>% k_argmax() %>% as.matrix()
  table(Predicted = pred, Actual = model.data$testtarget)

  # future-match class + probabilities -> prob_pred_df
  pred_new <- model %>% predict(model.data$future_matrix) %>% k_argmax() %>% as.matrix()
  prob_future <- model %>% predict(model.data$future_matrix)
  future_rows <- as.numeric(nrow(prob_future))
  prob_pred_future <- cbind(round(prob_future[1:future_rows, 1:model.data$test_dim], 3),
                            pred_new[1:future_rows])
  prob_pred_df <- as.data.frame(prob_pred_future)

  # predictions for all matches -> attach to future_data_lean
  data_mat <- rbind(model.data$data, model.data$full_future_matrix)
  x <- data_mat[, 2:col_num]
  all_match_pred <- model %>% predict(x)
  future_data_lean <- cbind(future_data_lean, all_match_pred)
  future_data_lean <- future_data_lean %>%
    rename(pred_loss_prob = `1`, pred_win_prob = `2`)

  # --- Primary model override ---
  # model_primary is loaded by the shell only when MODEL_BETLESS = FALSE; NULL skips.
  # Replaces betless predictions for upcoming matches that have odds; the rest keep betless.
  if (!is.null(model_primary) && !is.null(future_data_full)) {
    future_rows_with_odds <- future_data_full %>%
      filter(results == 999) %>%
      select(team, opposition, Season, status)

    if (nrow(future_rows_with_odds) > 0) {
      col_num_full <- ncol(future_data_full)
      data_full <- future_data_full
      data_full[1:col_num_full] <- lapply(data_full[1:col_num_full], as.numeric)
      model.data.full <- model_data(data_full)

      prob_primary <- model_primary %>% predict(model.data.full$future_matrix)
      primary_preds <- future_rows_with_odds %>%
        mutate(pred_loss_prob_primary = prob_primary[, 1],
               pred_win_prob_primary  = prob_primary[, 2])

      future_data_lean <- apply_primary_override(future_data_lean, primary_preds)

      n_primary  <- nrow(future_rows_with_odds)
      n_fallback <- sum(future_data_lean$results == 999) - n_primary
      message(n_primary, " match(es) predicted with primary model (", MODEL_PATH_FULL, "), ",
              n_fallback, " with betless fallback (", MODEL_PATH, ").")
    }
  }

  future_data_lean <- future_data_lean %>%
    mutate(pred_cat = ifelse(pred_win_prob < 0.1, 1,
                             ifelse(pred_win_prob > 0.1 & pred_win_prob < 0.2, 2,
                                    ifelse(pred_win_prob > 0.2 &pred_win_prob < 0.3, 3,
                                           ifelse(pred_win_prob > 0.3 & pred_win_prob < 0.4, 4,
                                                  ifelse(pred_win_prob > 0.4 & pred_win_prob < 0.5, 5,
                                                         ifelse(pred_win_prob > 0.5 & pred_win_prob < 0.6, 6,
                                                                ifelse(pred_win_prob > 0.6 & pred_win_prob< 0.7, 7,
                                                                       ifelse(pred_win_prob > 0.7 & pred_win_prob < 0.8, 8,
                                                                              ifelse(pred_win_prob > 0.8 & pred_win_prob < 0.9, 9, 10))))))))))
  #make numerical value categories
  future_data_lean$pred_cat_factor <- as.factor(future_data_lean$pred_cat)

  # New facet label names for supp variable
  future_data_lean$pred_cat <- factor(future_data_lean$pred_cat_factor, levels = c(1,2,3,4,5,6,7,8,9,10),
                            labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))

  # Find the mean of each group removing the unknown margins
  Sum_pred_cat <- future_data_lean %>%
    left_join(
      score_data_lean %>%
        select(Margin, Season, team, opposition, status, venue, matchType)
    ) %>%
    group_by(pred_cat) %>%
    filter(Margin != 999) %>%
    summarise(rating.mean=mean(Margin), rating.sd = sd(Margin))

  # linear formula for predicting margin from win probability
  margin_formula <- future_data_lean %>%
    left_join(
      score_data_lean %>%
        select(Margin, Season, team, opposition, status, venue, matchType)
    ) %>%
    filter(Margin < 998)
  formula <- lm(Margin ~ pred_win_prob, data= margin_formula)
  future_data_lean <- future_data_lean %>%
    mutate(margin_est_linear = predict(formula, newdata = future_data_lean))

  list(prob_pred_df = prob_pred_df, future_data_lean = future_data_lean)
}

# ---- IO shell: load the Keras models, then call the core --------------------
predict_round <- function(future_data_lean, score_data_lean, future_data_full = NULL) {
  # Baseline betless model (model/model_betless). When MODEL_BETLESS = FALSE the
  # primary odds-aware model (model/model) overrides matches with odds — see core.
  model <- load_model_tf(MODEL_PATH)

  model_primary <- NULL
  if (!MODEL_BETLESS) {
    model_primary <- tryCatch(
      load_model_tf(MODEL_PATH_FULL),
      error = function(e) {
        warning("Primary model '", MODEL_PATH_FULL, "' failed to load — using betless for all matches.")
        NULL
      }
    )
  }

  predict_round_core(future_data_lean, score_data_lean, future_data_full, model, model_primary)
}
