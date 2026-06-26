# tests/verify_store_golden.R — assert the refactored stage-4 core matches snapshot.
#
# Calls assemble_round_outputs() on the saved golden inputs and checks t /
# new_ratings / current_round / current_season are identical to the pre-refactor
# snapshot. Pure — no CSV writes, no reactable render, no Keras.
#
#   Rscript tests/verify_store_golden.R   (after capture_store_golden.R)

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(tidyverse); library(magrittr)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/Store_predictions.R")   # defines assemble_round_outputs + store_round

out <- assemble_round_outputs(
  readRDS("tests/golden/predict_future_data_lean.rds"),
  readRDS("tests/golden/out_score_data_lean.rds"),
  readRDS("tests/golden/out_round.rds"),
  readRDS("tests/golden/out_glicko_rate.rds"),
  readRDS("tests/golden/out_round.no.rds")
)

checks <- list(
  t              = all.equal(out$t,              readRDS("tests/golden/store_t.rds")),
  new_ratings    = all.equal(out$new_ratings,    readRDS("tests/golden/store_new_ratings.rds")),
  current_round  = all.equal(out$current_round,  readRDS("tests/golden/store_current_round.rds")),
  current_season = all.equal(out$current_season, readRDS("tests/golden/store_current_season.rds"))
)

ok <- TRUE
for (nm in names(checks)) {
  if (isTRUE(checks[[nm]])) {
    message(sprintf("PASS  %-16s identical", nm))
  } else {
    ok <- FALSE
    message(sprintf("FAIL  %-16s", nm)); message(paste0("        ", checks[[nm]], collapse = "\n"))
  }
}
if (!ok) stop("Stage-4 refactor changed an output — see FAIL rows above.")
message("\nAll outputs identical — stage-4 refactor is behaviour-preserving.")
