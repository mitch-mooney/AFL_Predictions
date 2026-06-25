# tests/check_fixture.R — validates the get_fixture() adapter.
#
# This is the genuinely new logic this session (column + team-name mapping over
# the AFL.com source), and it needs only the AFL fixture endpoint — no Squiggle,
# no heavy AFLTables/Footywire fetches. Safe to run anywhere.
#
#   Rscript tests/check_fixture.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(fitzRoy); library(dplyr); library(lubridate)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))

YEAR <- as.numeric(format(Sys.Date(), "%Y"))
fx <- get_fixture(YEAR, source = "AFL")

fail <- function(msg) { message("FAIL  ", msg); quit(status = 1) }

# 1. canonical columns
want <- c("Date", "Season", "Season.Game", "Round", "Home.Team", "Away.Team", "Venue")
if (!setequal(names(fx), want)) fail(paste("columns:", paste(names(fx), collapse = ", ")))

# 2. non-empty
if (nrow(fx) == 0) fail("fixture is empty")

# 3. every team maps to a canonical AFL_TEAMS name
teams   <- unique(c(fx$Home.Team, fx$Away.Team))
unknown <- setdiff(teams, AFL_TEAMS)
if (length(unknown) > 0) fail(paste("non-canonical team names:", paste(unknown, collapse = ", ")))

# 4. Date parses with the format downstream expects
d <- as.Date(fx$Date, format = "%Y-%m-%d %H:%M:%S")
if (any(is.na(d))) fail("Date column not parseable as '%Y-%m-%d %H:%M:%S'")

# 5. round auto-detection yields a finite upcoming round
round.no <- fx %>% filter(as.Date(Date) >= Sys.Date()) %>% pull(Round) %>% min()
if (!is.finite(round.no)) fail("no upcoming round detected (season may be complete)")

message(sprintf("PASS  get_fixture: %d games, %d teams all canonical, next round = %s",
                nrow(fx), length(teams), round.no))
