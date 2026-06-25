# feature_investigation.R — MANUAL ANALYSIS SCRIPT, not part of the weekly pipeline.
#
# Purpose: Evaluate which features contribute most to predictions, and whether
#          candidate features from extra_features.R are worth adding.
#
#   1. Point-biserial correlations — how strongly each feature correlates with winning
#   2. Permutation importance      — how much accuracy drops when each feature is shuffled
#   3. Bar chart visualisations of both
#
# Required in environment before running:
#   future_data_lean  — from source("source_code/AFL_data.R")
#   model             — from source("source_code/prediction_model.R")
#   model.data        — from source("source_code/prediction_model.R")
#
# Load example:
#   file.source <- list.files("functions/")
#   sapply(paste0("functions/", file.source), source)
#   source("source_code/AFL_data.R")
#   source("source_code/prediction_model.R")
#   source("analysis_code/feature_investigation.R")

library(tidyverse)
library(magrittr)

# ---- Guard: check required objects exist ------------------------------------
stopifnot(
  "future_data_lean not found — run AFL_data.R first"   = exists("future_data_lean"),
  "model not found — run prediction_model.R first"      = exists("model"),
  "model.data not found — run prediction_model.R first" = exists("model.data")
)

# ---- 1. Prepare analysis dataset --------------------------------------------
# Use only rows with known outcomes. Drop any prediction columns added by prediction_model.R.
analysis_df <- future_data_lean %>%
  filter(results %in% c(0, 1)) %>%
  select(-any_of(c("pred_loss_prob", "pred_win_prob", "pred_cat",
                   "pred_cat_factor", "margin_est_linear")))

feature_names <- colnames(analysis_df)[-1]   # column 1 is 'results'
n_features    <- length(feature_names)
outcome       <- as.numeric(analysis_df$results)

message("Analysing ", n_features, " features across ", nrow(analysis_df), " historical matches.")

# ---- 2. Point-biserial correlations -----------------------------------------
# Pearson correlation with a binary outcome == point-biserial correlation.
pb_cors <- vapply(feature_names, function(feat) {
  x <- as.numeric(analysis_df[[feat]])
  if (sd(x, na.rm = TRUE) == 0) return(NA_real_)
  cor(x, outcome, use = "complete.obs")
}, numeric(1))

pb_df <- data.frame(
  feature     = feature_names,
  correlation = pb_cors,
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(correlation)) %>%
  mutate(
    abs_cor   = abs(correlation),
    direction = ifelse(correlation >= 0, "Positive", "Negative")
  ) %>%
  arrange(desc(abs_cor))

message("\nTop 10 features by correlation strength:")
print(head(pb_df, 10), digits = 4)

# ---- 3. Permutation importance ----------------------------------------------
# Shuffles each feature in the normalised test matrix, re-predicts, measures accuracy drop.
# A larger drop = the model relies more on that feature.
#
# NOTE: Runs one predict() call per feature (~20 iterations). Expect 30–120 seconds.

baseline_pred <- model %>%
  predict(model.data$test) %>%
  k_argmax() %>%
  as.array()
baseline_acc <- mean(baseline_pred == model.data$testtarget)
message("\nBaseline test accuracy: ", round(baseline_acc * 100, 2), "%")
message("Running permutation importance (", n_features, " features)...")

set.seed(TRAIN_TEST_SEED)

perm_results <- vector("list", n_features)
for (i in seq_len(n_features)) {
  test_permuted       <- model.data$test
  test_permuted[, i]  <- sample(test_permuted[, i])

  perm_pred <- model %>% predict(test_permuted) %>% k_argmax() %>% as.array()
  perm_acc  <- mean(perm_pred == model.data$testtarget)

  perm_results[[i]] <- data.frame(
    feature  = feature_names[i],
    perm_acc = perm_acc,
    acc_drop = baseline_acc - perm_acc,
    stringsAsFactors = FALSE
  )
  message("  [", i, "/", n_features, "] ", feature_names[i],
          " — acc drop: ", round((baseline_acc - perm_acc) * 100, 2), "pp")
}

perm_df <- bind_rows(perm_results) %>% arrange(desc(acc_drop))

# ---- 4. Summary table -------------------------------------------------------
feature_summary <- pb_df %>%
  select(feature, correlation, abs_cor) %>%
  left_join(perm_df %>% select(feature, acc_drop), by = "feature") %>%
  mutate(
    acc_drop_pp = round(acc_drop * 100, 3),
    correlation = round(correlation, 4)
  ) %>%
  select(feature, correlation, acc_drop_pp) %>%
  arrange(desc(acc_drop_pp))

message("\nFeature summary (sorted by permutation importance):")
print(feature_summary, row.names = FALSE)

# ---- 5. Plots ---------------------------------------------------------------
p_cor <- pb_df %>%
  mutate(feature = factor(feature, levels = rev(pb_df$feature))) %>%
  ggplot(aes(x = feature, y = correlation, fill = direction)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Positive" = "#4a90d9", "Negative" = "#e05c5c"),
    guide  = guide_legend(title = NULL)
  ) +
  labs(
    title    = "Point-Biserial Correlation: Feature vs Win Outcome",
    subtitle = "Positive = higher value associated with winning",
    x = NULL, y = "Correlation coefficient"
  ) +
  theme_AFL() +
  theme(legend.position = "right")

p_perm <- perm_df %>%
  mutate(feature = factor(feature, levels = rev(perm_df$feature))) %>%
  ggplot(aes(x = feature, y = acc_drop * 100)) +
  geom_col(fill = "#4a90d9") +
  coord_flip() +
  labs(
    title    = "Permutation Feature Importance",
    subtitle = paste0("Baseline accuracy: ", round(baseline_acc * 100, 2),
                      "% — bar = accuracy drop when feature is randomly shuffled"),
    x = NULL, y = "Accuracy drop (percentage points)"
  ) +
  theme_AFL()

print(p_cor)
print(p_perm)
