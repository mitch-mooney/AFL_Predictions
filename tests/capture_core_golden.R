# tests/capture_core_golden.R — snapshot build_features_core inputs + outputs.
#
# Run BEFORE extracting expand_home_away(). verify_core_golden.R then feeds the
# saved inputs into the post-extraction core and asserts byte-identical outputs.
# Footywire fetch is skipped (dat is read straight from AFLstats.csv) so the
# snapshot is deterministic; the extraction only touches in-core logic anyway.
#
#   Rscript tests/capture_core_golden.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/AFL_data.R")   # loads libs + defines build_features_core/build_features

YEAR <- as.numeric(format(Sys.Date(), "%Y"))

fixture_all <- get_fixture(YEAR, source = "AFL")
round.no <- fixture_all %>% filter(as.Date(Date) >= Sys.Date()) %>% pull(Round) %>% min()
fixture  <- fixture_all %>% filter(Round == round.no)

dat <- read.csv("csv_files/AFLstats.csv")
dat <- dat %>% select(!X) %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

betting_odds <- read.csv("csv_files/betting_odds.csv")
results      <- fetch_results_afltables(season = START_SEASON:YEAR)

betting_join <- read.csv("csv_files/betting_odds_round.csv")
betting_join <- betting_join[, !names(betting_join) %in% "X"]

dir.create("tests/golden", recursive = TRUE, showWarnings = FALSE)
saveRDS(results,      "tests/golden/in_results.rds")
saveRDS(dat,          "tests/golden/in_dat.rds")
saveRDS(betting_odds, "tests/golden/in_betting_odds.rds")
saveRDS(fixture,      "tests/golden/in_fixture.rds")
saveRDS(round.no,     "tests/golden/in_round_no.rds")
saveRDS(betting_join, "tests/golden/in_betting_join.rds")

feat <- build_features_core(results, dat, betting_odds, fixture, round.no, betting_join)
for (nm in names(feat)) saveRDS(feat[[nm]], paste0("tests/golden/out_", nm, ".rds"))

message("Captured ", length(feat), " outputs + 6 inputs to tests/golden/")
