library(tidyverse)

file.source = list.files("functions/")
sapply(paste0('functions/', file.source), source)

# =============================================================================
# Stage 1 — current-round betting odds.
#
# Split into a pure core and a thin IO shell (mirrors build_features). The core
# turns the raw odds-API JSON into the betting_join frame with no IO, so the
# fragile JSON parsing (line_odds / h2h_odds) is testable on saved fixtures. The
# shell does the API key lookup, the two HTTP fetches, and the CSV write.
# =============================================================================

# ---- pure core: raw JSON -> betting_join (one row per team) ------------------
build_round_odds <- function(line_json, h2h_json) {
  # gather median line + h2h odds
  line.median <- line_odds(line_json)
  h2h.median  <- h2h_odds(h2h_json)

  # join and clean line and h2h
  betting.df <- line.median %>%
    left_join(h2h.median, by = c("team.a", "team.b", "Date", "Season")) %>%
    mutate(Home.Win.Odds = ifelse(team.a == Home.Team, median_team.a, median_team.b),
           Away.Win.Odds = ifelse(team.b == Away.Team, median_team.b, median_team.a),
           Home.Team = expand_odds_team_names(sapply(strsplit(Home.Team, " "), `[`, 1)),
           Away.Team = expand_odds_team_names(sapply(strsplit(Away.Team, " "), `[`, 1))) %>%
    select(Date, Home.Team, Away.Team, Home.Win.Odds, Away.Win.Odds, Home.Line.Odds, Away.Line.Odds)

  betting.df %>%
    expand_home_away(c("Date", "Home.Team"), pairs = list(
      Odds         = c("Home.Win.Odds",  "Away.Win.Odds"),
      line_Odds    = c("Home.Line.Odds", "Away.Line.Odds"),
      Opp_Odds     = c("Away.Win.Odds",  "Home.Win.Odds"),
      Opp_lineOdds = c("Away.Line.Odds", "Home.Line.Odds")
    )) %>%
    select(Team, Opposition, Status, Odds, line_Odds, Opp_Odds, Opp_lineOdds)
}

# ---- IO shell: fetch the JSON, call the core, write the round CSV ------------
fetch_round_odds <- function() {
  odds_api_key <- Sys.getenv("ODDS_API_KEY")
  if (odds_api_key == "") stop("ODDS_API_KEY environment variable not set. Add it to ~/.Renviron")
  odds_base <- paste0("https://api.the-odds-api.com/v4/sports/aussierules_afl/odds/?apiKey=",
                      odds_api_key, "&regions=au&markets=")
  line_json <- jsonlite::read_json(paste0(odds_base, "spreads"))
  h2h_json  <- jsonlite::read_json(paste0(odds_base, "h2h"))
  if (length(line_json) == 0) stop("Odds API returned empty spreads response — check API key/quota")
  if (length(h2h_json)  == 0) stop("Odds API returned empty h2h response — check API key/quota")

  betting_join <- build_round_odds(line_json, h2h_json)
  write.csv(betting_join, "csv_files/betting_odds_round.csv")
  betting_join
}
