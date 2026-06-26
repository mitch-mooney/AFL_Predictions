# tests/capture_store_odds_golden.R — snapshot stage Store_betting_odds BEFORE refactor.
#
# Runs the CURRENT Store_betting_odds.R on fixed inputs and saves the inputs +
# the resulting betting_csv. verify_store_odds_golden.R then feeds the saved inputs
# into the refactored archive_betting_odds() core and asserts byte-identical output.
# Needs network (AFLTables results). Writes betting_odds.csv — caller restores it.
#
#   Rscript tests/capture_store_odds_golden.R   (then: git checkout the csv)

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(tidyverse); library(fitzRoy); library(magrittr)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))   # to_afltables_names

YEAR             <- 2026
round.no         <- readRDS("tests/golden/out_round.no.rds")
betting_odds_raw <- read.csv("csv_files/betting_odds.csv")
betting_odds     <- betting_odds_raw   # the global the script mutates in place

source("source_code/Store_betting_odds.R")   # fetches results, reads betting_join, writes csv

saveRDS(results,          "tests/golden/sbo_results.rds")
saveRDS(betting_join,     "tests/golden/sbo_betting_join.rds")
saveRDS(betting_odds_raw, "tests/golden/sbo_betting_odds.rds")
saveRDS(round.no,         "tests/golden/sbo_round_no.rds")
saveRDS(betting_csv,      "tests/golden/sbo_betting_csv.rds")
message("Captured store-betting-odds golden: betting_csv ", nrow(betting_csv), " rows")
