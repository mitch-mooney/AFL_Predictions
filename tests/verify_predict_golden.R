# tests/verify_predict_golden.R — assert refactored stage 3 matches the snapshot.
#
# Runs the full predict_round() (shell loads the real models, core assembles) on
# the saved stage-2 outputs and checks prob_pred_df + the enriched future_data_lean
# are identical to the pre-refactor snapshot. Needs Keras + the committed models.
#
#   Rscript tests/verify_predict_golden.R   (after capture_predict_golden.R)

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(keras); library(tidyverse); library(magrittr)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/prediction_model.R")   # defines predict_round_core + predict_round

res <- predict_round(
  readRDS("tests/golden/out_future_data_lean.rds"),
  readRDS("tests/golden/out_score_data_lean.rds"),
  readRDS("tests/golden/out_future_data_full.rds")
)

ok <- TRUE
checks <- list(
  prob_pred_df     = all.equal(res$prob_pred_df,     readRDS("tests/golden/predict_prob_pred_df.rds")),
  future_data_lean = all.equal(res$future_data_lean, readRDS("tests/golden/predict_future_data_lean.rds"))
)
for (nm in names(checks)) {
  if (isTRUE(checks[[nm]])) {
    message(sprintf("PASS  %-18s identical", nm))
  } else {
    ok <- FALSE
    message(sprintf("FAIL  %-18s", nm))
    message(paste0("        ", checks[[nm]], collapse = "\n"))
  }
}

if (!ok) stop("Stage-3 refactor changed an output — see FAIL rows above.")
message("\nBoth outputs identical — stage-3 refactor is behaviour-preserving.")
