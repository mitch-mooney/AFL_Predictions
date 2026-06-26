# tests/capture_predict_golden.R — snapshot stage-3 output BEFORE refactor.
#
# Reuses the stage-2 golden outputs (build_features) as stage-3 inputs, runs the
# CURRENT prediction_model.R on them, and saves prob_pred_df + the enriched
# future_data_lean. verify_predict_golden.R then feeds the same inputs into the
# refactored predict_round_core() and asserts the two outputs match.
#
# Needs Keras + the committed model files. Run BEFORE refactoring:
#   Rscript tests/capture_predict_golden.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(keras); library(tidyverse); library(magrittr)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))

# stage-2 outputs -> stage-3 inputs
future_data_lean <- readRDS("tests/golden/out_future_data_lean.rds")
future_data_full <- readRDS("tests/golden/out_future_data_full.rds")
score_data_lean  <- readRDS("tests/golden/out_score_data_lean.rds")

source("source_code/prediction_model.R")   # current version: mutates future_data_lean, makes prob_pred_df

saveRDS(prob_pred_df,      "tests/golden/predict_prob_pred_df.rds")
saveRDS(future_data_lean,  "tests/golden/predict_future_data_lean.rds")
message("Captured stage-3 golden: prob_pred_df (", nrow(prob_pred_df),
        " rows) + future_data_lean (", nrow(future_data_lean), " rows)")
