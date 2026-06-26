# tests/capture_store_golden.R — snapshot stage-4 assembly output BEFORE refactor.
#
# Runs the CURRENT Store_predictions.R (ground truth) on the saved golden inputs
# and captures the assembled t / new_ratings / current_round / current_season.
# Sourcing also writes the two archive CSVs and renders the reactable — wrapped in
# tryCatch so a render hiccup still lets us grab the globals; the caller restores
# the CSVs afterwards (git checkout).
#
#   Rscript tests/capture_store_golden.R   (then: git checkout the two CSVs)

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(tidyverse); library(magrittr)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))

# golden inputs (from earlier stage captures)
future_data_lean <- readRDS("tests/golden/predict_future_data_lean.rds")
score_data_lean  <- readRDS("tests/golden/out_score_data_lean.rds")
round            <- readRDS("tests/golden/out_round.rds")
glicko_rate      <- readRDS("tests/golden/out_glicko_rate.rds")
round.no         <- readRDS("tests/golden/out_round.no.rds")
prob_pred_df     <- readRDS("tests/golden/predict_prob_pred_df.rds")  # for the L9 rename

tryCatch(
  source("source_code/Store_predictions.R"),
  error = function(e) message("source halted (ok if at reactable render): ", conditionMessage(e))
)

saveRDS(t,              "tests/golden/store_t.rds")
saveRDS(new_ratings,    "tests/golden/store_new_ratings.rds")
saveRDS(current_round,  "tests/golden/store_current_round.rds")
saveRDS(current_season, "tests/golden/store_current_season.rds")
message("Captured stage-4 golden: t (", nrow(t), " rows) + new_ratings (", nrow(new_ratings), " rows)")
