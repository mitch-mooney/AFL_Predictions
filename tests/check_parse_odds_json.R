# tests/check_parse_odds_json.R — unit test for the merged odds-JSON parser.
# Hermetic: hand-built odds-api-shaped list, no network.
#
#   Rscript tests/check_parse_odds_json.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages({library(dplyr); library(tibble)})
invisible(sapply(paste0("functions/", list.files("functions/")), source))

# one game, two bookmakers, each with h2h + spreads markets
book <- function(h2h_a, h2h_b, sp_a, sp_b) list(title = "bk", markets = list(
  list(key = "h2h",     outcomes = list(list(name = "Carlton", price = h2h_a), list(name = "Essendon", price = h2h_b))),
  list(key = "spreads", outcomes = list(list(name = "Carlton", point = sp_a), list(name = "Essendon", point = sp_b)))
))
json <- list(list(
  home_team = "Carlton", away_team = "Essendon",
  commence_time = "2026-03-05T08:30:00Z",
  bookmakers = list(book(1.5, 2.6, -10.5, 10.5), book(1.7, 2.4, -12.5, 12.5))
))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

h2h <- parse_odds_json(json, "h2h", "price")
if (nrow(h2h) != 1) fail("h2h: expected 1 row")
if (!isTRUE(all.equal(h2h$median_team.a, median(c(1.5, 1.7))))) fail("h2h median home wrong")  # 1.6
if (!isTRUE(all.equal(h2h$median_team.b, median(c(2.6, 2.4))))) fail("h2h median away wrong")  # 2.5
if (h2h$team.a != "Carlton" || h2h$team.b != "Essendon") fail("h2h teams wrong")
if (h2h$Season != "2026") fail("Season wrong")

line <- parse_odds_json(json, "spreads", "point")
if (!isTRUE(all.equal(line$median_team.a, median(c(-10.5, -12.5))))) fail("line median home wrong")  # -11.5

# wrappers preserve the old per-market shapes
lo <- line_odds(json)
if (!all(c("Home.Line.Odds", "Away.Line.Odds") %in% names(lo))) fail("line_odds shape wrong")
if (!isTRUE(all.equal(lo$Home.Line.Odds, -11.5))) fail("line_odds Home.Line.Odds wrong")

# missing market -> game skipped
no_h2h <- list(list(home_team = "A", away_team = "B", commence_time = "2026-03-05T08:30:00Z",
  bookmakers = list(list(title = "bk", markets = list(
    list(key = "spreads", outcomes = list(list(name = "A", point = -5), list(name = "B", point = 5))))))))
if (nrow(parse_odds_json(no_h2h, "h2h", "price")) != 0) fail("missing h2h market should skip game")

message("PASS  parse_odds_json: medians, value field, market filter, wrappers, skip-on-missing")
