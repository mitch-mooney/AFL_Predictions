## Archive a completed round's results + odds into csv_files/betting_odds.csv.
## Manual post-results step (not part of run_pipeline). After a round's results
## are in:  source("source_code/Store_betting_odds.R"); store_betting_odds(round.no)
library(tidyverse)
library(fitzRoy)
library(data.table)
library(PlayerRatings)
library(plotly)
library(lubridate)
library(reshape2)
library(ggpmisc)
library(magrittr)

# =============================================================================
# Split into a pure core and a thin IO shell (mirrors the pipeline stages). The
# core joins the round's odds (betting_join) to its results and appends them to
# the existing archive; the shell fetches results and reads/writes the CSVs.
# =============================================================================

# ---- pure core: results + round odds + existing archive -> updated archive ---
archive_betting_odds <- function(results, betting_join, betting_odds, year, round.no) {
  newest.results <- results %>%
    filter(Season == year, Round.Number == round.no+1) %>%
    select(Date, Venue, Round.Number, Home.Team, Away.Team, Home.Points, Away.Points)

  newest.betting <- betting_join %>%
    filter(Status == "Home") %>%
    select(!Status) %>%
    rename(Home.Team = Team,
           Away.Team = Opposition) %>%
    mutate(Home.Team = to_afltables_names(Home.Team),
           Away.Team = to_afltables_names(Away.Team)) %>%
    left_join(newest.results, by = c("Home.Team", "Away.Team"))

  newest.betting <- newest.betting %>%
    mutate(Home.Margin = Home.Points - Away.Points,
           Away.Margin = Away.Points - Home.Points,
           Season = as.integer(format(Date, "%Y")),
           Date = format(Date, format = "%d/%m/%Y")) %>%
    rename(Round = Round.Number,
           Home.Score = Home.Points,
           Away.Score = Away.Points,
           Home.Win.Odds = Odds,
           Away.Win.Odds = Opp_Odds,
           Home.Line.Odds = line_Odds,
           Away.Line.Odds = Opp_lineOdds) %>%
    select("Date", "Venue", "Home.Team", "Away.Team","Home.Score","Away.Score", "Home.Margin", "Away.Margin","Home.Win.Odds", "Away.Win.Odds", "Home.Line.Odds", "Away.Line.Odds", "Round", "Season") %>%
    drop_na(Date)

  betting_odds <- betting_odds %>%
    select(-starts_with("X")) %>%
    unique() %>%
    drop_na(Date)

  rbind(betting_odds, newest.betting) %>%
    drop_na(Date)
}

# ---- IO shell: fetch results, read/write the archives, call the core ---------
store_betting_odds <- function(round.no,
                               year = as.numeric(format(Sys.Date(), "%Y")),
                               betting_join = NULL) {
  results      <- fetch_results_afltables(season = year)
  betting_odds <- read.csv("csv_files/betting_odds.csv")
  if (is.null(betting_join)) {
    betting_join <- read.csv("csv_files/betting_odds_round.csv") %>% select(-X)
  }

  betting_csv <- archive_betting_odds(results, betting_join, betting_odds, year, round.no)
  write.csv(betting_csv, "csv_files/betting_odds.csv")
  betting_csv
}
