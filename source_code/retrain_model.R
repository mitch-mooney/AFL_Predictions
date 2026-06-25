# retrain_model.R — Retrain the Keras model on current data and update model/model_betless
#
# Run this when:
#   - Several new seasons of data have been added
#   - You've added or removed features from future_data_lean
#   - You want to experiment with hyperparameters (edit functions/config.R first)
#
# Dependencies (must be in environment before running):
#   future_data_lean — from source("source_code/AFL_data.R")
#   All functions/   — loaded via AFL_data.R or manually:
#                      sapply(paste0("functions/", list.files("functions/")), source)
#
# Usage:
#   source("source_code/AFL_data.R")
#   source("source_code/retrain_model.R")

library(keras)

# ---- 1. Prepare data --------------------------------------------------------
data    <- if (MODEL_BETLESS) future_data_lean %>% select(-matchType) else future_data_full
# Save the retrained model to the path matching the feature set it was trained on:
# betless features -> MODEL_PATH (model/model_betless), full features -> MODEL_PATH_FULL (model/model).
active_model_path <- if (MODEL_BETLESS) MODEL_PATH else MODEL_PATH_FULL
model_label       <- if (MODEL_BETLESS) "betless" else "full"
col_num <- as.numeric(ncol(data))   # must be set before model_training() reads it as a global
data[1:col_num] <- lapply(data[1:col_num], as.numeric)

model.data <- model_data(data)

message("Training on ", nrow(model.data$training), " samples | ",
        "Testing on ", model.data$test_var, " samples | ",
        col_num - 1, " features")
message("Max epochs: ", MODEL_EPOCHS, " | Early stopping patience: ", MODEL_PATIENCE)

# ---- 2. Train ---------------------------------------------------------------
model_new <- model_training(
  inputs = model.data$full_data_matrix,
  target = model.data$full_data_target
)

# ---- 3. Evaluate on held-out test set ---------------------------------------
eval_results  <- model_new %>% evaluate(model.data$test, model.data$testLabels, verbose = 0)
test_accuracy <- round(eval_results["accuracy"] * 100, 2)
test_loss     <- round(eval_results["loss"], 4)

message("Test accuracy: ", test_accuracy, "%  |  Test loss: ", test_loss)

# Confusion matrix
pred_classes <- model_new %>%
  predict(model.data$test) %>%
  k_argmax() %>%
  as.matrix()
message("Confusion matrix:")
print(table(Predicted = pred_classes, Actual = model.data$testtarget))

# ---- 4. Save timestamped archive --------------------------------------------
timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
acc_str       <- gsub("\\.", "p", as.character(test_accuracy))
archive_dir   <- file.path("model", "archive")

if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)

archive_path <- file.path(archive_dir,
                           paste0("model_", model_label, "_", timestamp_str, "_acc", acc_str))
save_model_tf(model_new, archive_path)
message("Archived to: ", archive_path)

# ---- 5. Update training log -------------------------------------------------
log_path  <- file.path("csv_files", "training_log.csv")
log_entry <- data.frame(
  timestamp        = timestamp_str,
  n_train          = nrow(model.data$training),
  n_test           = model.data$test_var,
  n_features       = col_num - 1L,
  test_accuracy    = test_accuracy,
  test_loss        = test_loss,
  units            = paste(MODEL_UNITS,   collapse = "-"),
  dropout          = paste(MODEL_DROPOUT, collapse = "-"),
  max_epochs       = MODEL_EPOCHS,
  patience         = MODEL_PATIENCE,
  learning_rate    = MODEL_LR,
  batch_size       = MODEL_BATCH,
  archive_path     = archive_path,
  stringsAsFactors = FALSE
)

if (file.exists(log_path)) {
  existing <- read.csv(log_path, stringsAsFactors = FALSE)
  existing <- existing[, !names(existing) %in% "X"]   # drop row index if present
  log_entry <- rbind(existing, log_entry)
}
write.csv(log_entry, log_path, row.names = FALSE)
message("Training log updated: ", log_path)

# ---- 6. Overwrite active model ----------------------------------------------
# In interactive sessions, ask before overwriting. In non-interactive (scheduled jobs), proceed.
overwrite <- TRUE
if (interactive()) {
  answer <- readline(prompt = paste0(
    "\nOverwrite active ", model_label, " model at '", active_model_path, "' with new model (",
    test_accuracy, "% accuracy)? [y/N]: "
  ))
  overwrite <- tolower(trimws(answer)) == "y"
}

if (overwrite) {
  save_model_tf(model_new, active_model_path)
  message("Active model updated: ", active_model_path)
  message("Retraining complete.")
} else {
  message("Active model NOT updated. Archived model available at: ", archive_path)
  message("To activate manually: save_model_tf(model_new, \"", active_model_path, "\")")
}
