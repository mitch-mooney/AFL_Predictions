# tests/check_build_features.R — full live run of build_features() with invariant
# checks. With Squiggle dead there is no pre-refactor baseline to diff against, so
# this asserts the outputs are well-formed rather than byte-identical to before.
#
# Needs network (AFLTables results; Footywire stats fall back to the CSV on failure)
# and reads csv_files/betting_odds_round.csv for current-round odds. Run in a stable
# R environment:  Rscript tests/check_build_features.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/AFL_data.R")   # defines build_features_core() + build_features()

feat <- build_features(betting_join = NULL)   # NULL -> CSV fallback for odds

rnd <- feat$round
fdl <- feat$future_data_lean

checks <- c(
  "round.no is a finite round number"        = is.finite(feat$round.no),
  "round has an even, non-zero row count"    = nrow(rnd) > 0 && nrow(rnd) %% 2 == 0,
  "all round teams are canonical"            = length(setdiff(unique(c(rnd$Team, rnd$Opposition)), AFL_TEAMS)) == 0,
  "round Home/Away balanced"                 = sum(rnd$Status == "Home") == sum(rnd$Status == "Away"),
  "upcoming rows present (results == 999)"   = sum(fdl$results == 999) > 0,
  "no NA team/opposition encoding upcoming"  = !any(is.na(fdl$team[fdl$results == 999])) &&
                                               !any(is.na(fdl$opposition[fdl$results == 999])),
  "score_data_lean non-empty"                = nrow(feat$score_data_lean) > 0,
  "future_data_full non-empty"               = nrow(feat$future_data_full) > 0,
  "glicko_rate covers >= 18 teams"           = length(unique(feat$glicko_rate$ratings$Player)) >= 18
)

ok <- TRUE
for (nm in names(checks)) {
  if (isTRUE(checks[[nm]])) {
    message(sprintf("PASS  %s", nm))
  } else {
    ok <- FALSE
    message(sprintf("FAIL  %s", nm))
  }
}

message(sprintf("\nDetected round %s | round rows: %d | upcoming team-rows: %d",
                feat$round.no, nrow(rnd), sum(fdl$results == 999)))

if (!ok) stop("build_features() produced malformed output — see FAIL rows above.")
message("All invariants hold — build_features() output is well-formed.")
