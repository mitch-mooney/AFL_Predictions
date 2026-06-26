# tests/capture_odds_golden.R — snapshot stage-1 inputs + output BEFORE refactor.
#
# One live odds-API call: saves the raw spreads + h2h JSON and the resulting
# betting_join (computed with the current inline logic). verify_odds_golden.R then
# feeds the saved JSON into the refactored build_round_odds() and asserts the
# betting_join matches — drift-proof against the API returning different odds.
#
#   Rscript tests/capture_odds_golden.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(tidyverse))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

odds_api_key <- Sys.getenv("ODDS_API_KEY")
if (odds_api_key == "") stop("ODDS_API_KEY not set")
odds_base <- paste0("https://api.the-odds-api.com/v4/sports/aussierules_afl/odds/?apiKey=",
                    odds_api_key, "&regions=au&markets=")
line.odds_json <- jsonlite::read_json(paste0(odds_base, "spreads"))
h2h.odds_json  <- jsonlite::read_json(paste0(odds_base, "h2h"))
stopifnot(length(line.odds_json) > 0, length(h2h.odds_json) > 0)

# --- current inline logic (betting_odds.R lines 14-33) ---
line.median <- line_odds(line.odds_json)
h2h.median  <- h2h_odds(h2h.odds_json)
betting.df <- line.median %>%
  left_join(h2h.median, by = c("team.a", "team.b", "Date", "Season")) %>%
  mutate(Home.Win.Odds = ifelse(team.a == Home.Team, median_team.a, median_team.b),
         Away.Win.Odds = ifelse(team.b == Away.Team, median_team.b, median_team.a),
         Home.Team = expand_odds_team_names(sapply(strsplit(Home.Team, " "), `[`, 1)),
         Away.Team = expand_odds_team_names(sapply(strsplit(Away.Team, " "), `[`, 1))) %>%
  select(Date, Home.Team, Away.Team, Home.Win.Odds, Away.Win.Odds, Home.Line.Odds, Away.Line.Odds)
betting_join <- betting.df %>%
  expand_home_away(c("Date", "Home.Team"), pairs = list(
    Odds         = c("Home.Win.Odds",  "Away.Win.Odds"),
    line_Odds    = c("Home.Line.Odds", "Away.Line.Odds"),
    Opp_Odds     = c("Away.Win.Odds",  "Home.Win.Odds"),
    Opp_lineOdds = c("Away.Line.Odds", "Home.Line.Odds")
  )) %>%
  select(Team, Opposition, Status, Odds, line_Odds, Opp_Odds, Opp_lineOdds)

dir.create("tests/golden", recursive = TRUE, showWarnings = FALSE)
saveRDS(line.odds_json, "tests/golden/odds_line_json.rds")
saveRDS(h2h.odds_json,  "tests/golden/odds_h2h_json.rds")
saveRDS(betting_join,   "tests/golden/odds_betting_join.rds")
message("Captured odds golden: ", nrow(betting_join), " rows")
