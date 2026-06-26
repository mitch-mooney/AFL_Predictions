# tests/verify_store_odds_golden.R — assert the refactored core matches snapshot.
#
# Feeds the saved golden inputs into archive_betting_odds() and checks betting_csv
# is byte-identical to the pre-refactor snapshot. Offline — no fetch, no write.
#
#   Rscript tests/verify_store_odds_golden.R   (after capture_store_odds_golden.R)

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(tidyverse); library(magrittr)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/Store_betting_odds.R")   # defines archive_betting_odds + store_betting_odds

betting_csv <- archive_betting_odds(
  results      = readRDS("tests/golden/sbo_results.rds"),
  betting_join = readRDS("tests/golden/sbo_betting_join.rds"),
  betting_odds = readRDS("tests/golden/sbo_betting_odds.rds"),
  year         = 2026,
  round.no     = readRDS("tests/golden/sbo_round_no.rds")
)

cmp <- all.equal(betting_csv, readRDS("tests/golden/sbo_betting_csv.rds"))
if (isTRUE(cmp)) {
  message("PASS  betting_csv identical — Store_betting_odds refactor is behaviour-preserving.")
} else {
  message("FAIL  betting_csv differs:")
  message(paste0("  ", cmp, collapse = "\n"))
  quit(status = 1)
}
