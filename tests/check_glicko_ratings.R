# tests/check_glicko_ratings.R — regression test for the zero-rating-change fix.
#
# Engineers the exact case the old apply(glicko != 0) filter dropped: a first-game
# draw between two teams at the default rating produces EXACTLY zero rating change
# (expected score 0.5 == actual 0.5). The old proxy would drop that game and shift
# the team's match_num; the schedule-derived selection keeps it.
#
#   Rscript tests/check_glicko_ratings.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(PlayerRatings); library(dplyr); library(magrittr)
                  library(data.table); library(reshape2)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))

# Home-perspective schedule (glicko_ratings only reads Status == 'Home' rows).
# d1: A vs B is a DRAW (results = 2) -> both stay at default -> zero rating change.
match <- data.frame(
  Date       = as.Date(c("2020-01-01", "2020-01-08", "2020-01-15", "2020-01-22")),
  Status     = "Home",
  Team       = c("A", "A", "B", "A"),
  Opposition = c("B", "C", "C", "B"),
  results    = c(2L, 1L, 1L, 1L),   # 2 = draw
  stringsAsFactors = FALSE
)

g <- glicko_ratings(match)
clean <- g$clean

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# played pairs: A in {d1,d2,d4}=3, B in {d1,d3,d4}=3, C in {d2,d3}=2  -> 8 total
if (nrow(clean) != 8) fail(paste("expected 8 rating rows (one per team-game), got", nrow(clean)))

a <- clean %>% filter(Team == "A") %>% arrange(match_num)
if (nrow(a) != 3) fail(paste("team A should have 3 games, got", nrow(a)))
if (!identical(a$match_num, 1:3)) fail(paste("team A match_num not 1..3:", paste(a$match_num, collapse = ",")))

# The crux: A's first game (the draw) must survive with zero rate_change.
if (a$rate_change[1] != 0) fail(paste("draw game should have rate_change 0, got", a$rate_change[1]))
if (a$match_num[1] != 1)   fail("draw game must be A's match_num == 1 (not dropped/shifted)")

bc <- clean %>% group_by(Team) %>% summarise(n = n(), .groups = "drop")
if (!setequal(bc$n[bc$Team == "B"], 3) || !setequal(bc$n[bc$Team == "C"], 2)) fail("B/C game counts wrong")

message("PASS  glicko_ratings: zero-change draw retained; match_num aligns 1..N per team")
