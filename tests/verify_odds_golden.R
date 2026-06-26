# tests/verify_odds_golden.R — assert refactored build_round_odds matches snapshot.
#
# Feeds the saved raw JSON into build_round_odds() and checks the betting_join is
# byte-identical to the pre-refactor snapshot. Offline — no API call.
#
#   Rscript tests/verify_odds_golden.R   (after capture_odds_golden.R)

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(tidyverse))
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/betting_odds.R")   # bootstrap + defines build_round_odds/fetch_round_odds (no API call)

bj <- build_round_odds(
  readRDS("tests/golden/odds_line_json.rds"),
  readRDS("tests/golden/odds_h2h_json.rds")
)

cmp <- all.equal(bj, readRDS("tests/golden/odds_betting_join.rds"))
if (isTRUE(cmp)) {
  message("PASS  betting_join identical — stage-1 refactor is behaviour-preserving.")
} else {
  message("FAIL  betting_join differs:")
  message(paste0("  ", cmp, collapse = "\n"))
  quit(status = 1)
}
