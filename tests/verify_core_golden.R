# tests/verify_core_golden.R — assert the post-extraction core matches the snapshot.
#
# Feeds the saved golden inputs into build_features_core() and checks each of the
# six outputs is byte-identical to the pre-extraction snapshot. Pure, offline,
# instant. Run AFTER capture_core_golden.R and after each extraction step.
#
#   Rscript tests/verify_core_golden.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/AFL_data.R")

feat <- build_features_core(
  results      = readRDS("tests/golden/in_results.rds"),
  dat          = readRDS("tests/golden/in_dat.rds"),
  betting_odds = readRDS("tests/golden/in_betting_odds.rds"),
  fixture      = readRDS("tests/golden/in_fixture.rds"),
  round.no     = readRDS("tests/golden/in_round_no.rds"),
  betting_join = readRDS("tests/golden/in_betting_join.rds")
)

ok <- TRUE
for (nm in names(feat)) {
  cmp <- all.equal(feat[[nm]], readRDS(paste0("tests/golden/out_", nm, ".rds")))
  if (isTRUE(cmp)) {
    message(sprintf("PASS  %-18s identical", nm))
  } else {
    ok <- FALSE
    message(sprintf("FAIL  %-18s", nm))
    message(paste0("        ", cmp, collapse = "\n"))
  }
}

if (!ok) stop("Extraction changed an output — see FAIL rows above.")
message("\nAll six outputs identical — extraction is behaviour-preserving.")
