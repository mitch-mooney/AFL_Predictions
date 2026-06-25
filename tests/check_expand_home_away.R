# tests/check_expand_home_away.R — synthetic unit test for the helper.
#
# Offline, fast, runs anywhere. Pins the Home/Away skeleton and the column-pair
# swap on a hand-built 2-match frame — covers the betting_odds.R reshape (stage 1)
# that the core golden test does not exercise.
#
#   Rscript tests/check_expand_home_away.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(dplyr))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

df <- data.frame(
  Date           = c("2026-01-01", "2026-01-02"),
  Home.Team      = c("Carlton", "Geelong"),
  Away.Team      = c("Essendon", "Sydney"),
  Home.Win.Odds  = c(1.5, 2.0),
  Away.Win.Odds  = c(2.6, 1.8),
  Home.Line.Odds = c(1.9, 1.85),
  Away.Line.Odds = c(1.9, 1.95),
  stringsAsFactors = FALSE
)

out <- expand_home_away(df, c("Date", "Home.Team"), pairs = list(
  Odds         = c("Home.Win.Odds",  "Away.Win.Odds"),
  Opp_Odds     = c("Away.Win.Odds",  "Home.Win.Odds"),
  line_Odds    = c("Home.Line.Odds", "Away.Line.Odds"),
  Opp_lineOdds = c("Away.Line.Odds", "Home.Line.Odds")
))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

if (nrow(out) != 4) fail(paste("expected 4 rows, got", nrow(out)))

h1 <- out[out$Team == "Carlton", ]   # Home row, match 1
if (h1$Status     != "Home")     fail("Home Status")
if (h1$Opposition != "Essendon") fail("Home Opposition")
if (h1$Odds       != 1.5)        fail("Home own Odds")
if (h1$Opp_Odds   != 2.6)        fail("Home Opp_Odds")
if (h1$line_Odds  != 1.9)        fail("Home line_Odds")

a1 <- out[out$Team == "Essendon", ]  # Away row, match 1
if (a1$Status     != "Away")     fail("Away Status")
if (a1$Opposition != "Carlton")  fail("Away Opposition")
if (a1$Odds       != 2.6)        fail("Away own Odds")
if (a1$Opp_Odds   != 1.5)        fail("Away Opp_Odds")

message("PASS  expand_home_away: 4 rows, Home/Away skeleton + odds swap correct")
