library(fitzRoy)
library(data.table)
library(PlayerRatings)
library(plotly)
library(lubridate)
library(reshape2)
library(ggpmisc)
library(magrittr)
library(dplyr)     # build_features uses dplyr verbs directly — declare it here
                   # rather than rely on betting_odds.R having loaded tidyverse first

# =============================================================================
# build_features — stage 2 of the pipeline.
#
# Split into a pure core and a thin IO shell (see architecture review, candidate
# #2). build_features_core does all the join / Glicko / feature-lag logic with no
# IO, so it is testable through its interface. build_features() is the adapter
# that performs every read/fetch/write, then calls the core.
#
# Behaviour is preserved verbatim from the previous top-level script — the only
# deviations are mechanical ones forced by moving the body into a function:
#   * wrangle_fixture() is now passed its `data` explicitly (it used to read a
#     global `fixture`).
#   * betting_join arrives as a parameter; the IO shell resolves it (in-memory
#     arg or CSV fallback) instead of the core touching disk.
#
# The fixture comes from get_fixture() (functions/get_fixture.R), an adapter over
# fitzRoy's AFL.com source. It replaced fetch_fixture_squiggle(), which is broken
# against current curl (curl_parse_url no longer exported).
# =============================================================================

# ---- pure core: all transformation, no IO -----------------------------------
# Inputs are already-materialised data frames; returns a named list of the six
# outputs the downstream stages consume.
build_features_core <- function(results, dat, betting_odds, fixture, round.no,
                                betting_join = NULL) {

  ##########----- Clean and merge results with stats -----##########

  # One row per team, swapping the home/away score columns. (filter first — Season
  # drops whole matches, so it is equivalent to the old in-group filter.)
  res <- results %>%
    filter(Season >= START_SEASON) %>%
    expand_home_away("Game", pairs = list(
      goals       = c("Home.Goals",   "Away.Goals"),
      behinds     = c("Home.Behinds", "Away.Behinds"),
      points      = c("Home.Points",  "Away.Points"),
      opp_goals   = c("Away.Goals",   "Home.Goals"),
      opp_behinds = c("Away.Behinds", "Home.Behinds"),
      opp_points  = c("Away.Points",  "Home.Points")
    )) %>%
    mutate(Margin = points - opp_points) %>%
    select(Date, Season, Team, goals, behinds, points, opp_goals, opp_behinds, opp_points, Margin)
  # clean team names
  res$Team <- normalize_team_names(res$Team)

  # get team summarized data for merging
  match<-dat %>%
    group_by(Date, Season, Round,Venue, Team, Opposition, Status, Match_id)%>%
    summarise_if(is.numeric, sum, na.rm=TRUE)

  #match <- match %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
  # bind res with match
  match<-merge(match, res, by=c("Date","Season", "Team"))

  #differential scores
  match %<>%
    group_by(Match_id) %>%
    mutate(tackle_diff = (T*2) - sum(T),
           SC_diff = (SC*2)- sum(SC),
           score_acc = G/(G+B)) %>%
    ungroup()

  #turn score difference into an integer D = 2, W = 1, L = 0
  match$results <- ifelse(match$Margin < 0, 0, ifelse(match$Margin > 0, 1, 2))
  # determine how many wins had for the year
  match %<>%
    group_by(Season, Team) %>%
    arrange(Date)%>%
    mutate(wins_this_season = cumsum(ifelse(results == 2, 0.5, results)))%>%
    ungroup()

  ##########----- Make Glicko Ratings -----##########
  g <- glicko_ratings(match)
  glicko_rate  <- g$rate     # returned output
  glicko_clean <- g$clean    # merged into match below

  #join with match dataset
  match$date <- as.integer(format(match$Date, "%Y%m%d"))
  match %<>%
    group_by(Team) %>%
    mutate(match_num = order(order(date, decreasing=F)))

  match <- merge(match, glicko_clean, by=c("Team","match_num"))

  ##########----- Clean and merge betting statistics -----##########

  # One row per team, swapping the home/away odds columns (see expand_home_away).
  bet <- betting_odds %>%
    expand_home_away(c("X", "Home.Team"), pairs = list(
      Odds         = c("Home.Win.Odds",  "Away.Win.Odds"),
      Opp_Odds     = c("Away.Win.Odds",  "Home.Win.Odds"),
      line_Odds    = c("Home.Line.Odds", "Away.Line.Odds"),
      Opp_lineOdds = c("Away.Line.Odds", "Home.Line.Odds")
    )) %>%
    select(Date, Status, Home.Team, Team, Odds, Opp_Odds, line_Odds, Opp_lineOdds) %>%
    distinct()

  #clean up team names
  bet$Team <- normalize_team_names(bet$Team)

  #If you have to read in the .csv locally you'll have to change the Date column to date format
  #bet$Date <- strptime(as.character(bet$Date), "%d/%m/%Y")
  bet$Date <- as.Date(bet$Date,format = "%d/%m/%Y")

  #merge with match stats
  match <- dplyr::inner_join(match, bet, by=c("Date","Status", "Team"))

  ##########----- Add next round fixture to dataframe -----##########

  # add new fixture to dataframe for prediction
  round <- wrangle_fixture(round = round.no, data = fixture)
  #round <- readr::read_csv('csv_files/fixture.csv')
  # change date format
  round$Date<- as.Date(round$Date,format = "%Y-%m-%d %H:%M:%S")
  # clean up strings
  round <- round %>%
    select(Date, Match_id, Match_id, Season, Team, Opposition, Status, Venue, Round, results, Margin)

  # Join current round odds if available (resolved by the IO shell and passed in).
  if (!is.null(betting_join) && nrow(betting_join) > 0) {
    round <- round %>% left_join(betting_join, by = c('Team', 'Opposition', 'Status'))
  } else {
    warning("betting_join not found — run betting_odds.R before build_features() for odds data. ",
            "Proceeding without current round odds (Odds/line_Odds will be NA in output).")
    round <- round %>%
      mutate(Odds = NA_real_, line_Odds = NA_real_, Opp_Odds = NA_real_, Opp_lineOdds = NA_real_)
  }

  #bind rows need to use plyr to fill blank columns
  new<-plyr::rbind.fill(match, round)

  #change team names & home and away status to integer values
  new$team       <- as.numeric(ordered(new$Team,       levels = AFL_TEAMS))
  new$opposition <- as.numeric(ordered(new$Opposition, levels = AFL_TEAMS))
  new$status <- as.numeric(ordered(new$Status, levels = c("Home", "Away")))

  new$matchType <- ifelse(grepl('Final', new$Round), 1, 0)
  #new$date <- as.integer(format(new$Date, "%Y%m%d"))
  #finalize the variable lists for modeling
  new <- new %>%
    arrange(Date) %>%
    add_lag_features("Team", c(
      last_scoreDiff      = "Margin",
      last_result         = "results",
      last_SC             = "SC_diff",
      last_score_acc      = "score_acc",
      last_disposals      = "D",
      last_I50            = "I50",
      last_One.Percenters = "One.Percenters",
      pre_rate            = "value",
      last_tackleDiff     = "tackle_diff",
      matches_won         = "wins_this_season"
    ))

  new %<>%
    group_by(Season, Team) %>%
    mutate(season_for = lag(cumsum(points),1,default = 0),
           season_against = lag(cumsum(opp_points),1,default = 0))

  new %<>%
    group_by(Match_id) %>%
    mutate(rate_diff = (pre_rate*2)-sum(pre_rate),
           opp_rating = (sum(pre_rate)-pre_rate),
           opp_season_for = (sum(season_for)-season_for),
           opp_season_against = (sum(season_against)-season_against)) %>%
    ungroup()

  new %<>%
    group_by(Team, Opposition) %>%
    mutate(last_encounter_margin = lag(Margin, order_by = date),
           last_encounter_SC = lag(SC, order_by = Date),
           last_encounter_disposals = lag(D, order_by=date),
           last_encounter_line_Odds = lag(line_Odds, order_by = date)) %>%
    ungroup()

  # use above metrics to create a couple of final variables.
  # last_oppRate lags pre_oppRate, created earlier in the same call (order matters).
  new <- new %>%
    add_lag_features("Team", c(
      last_rateDiff = "rate_diff",
      pre_oppRate   = "opp_rating",
      last_opp      = "opposition",
      last_oppRate  = "pre_oppRate",
      last_Odds     = "Odds",
      last_LineOdds = "line_Odds",
      last_CP       = "CP",
      last_CM       = "CM",
      last_MI5      = "MI5",
      last_AF       = "AF"
    ))
  # venue is a PER-TEAM factor encoding — keep it inside group_by(Team).
  new <- new %>%
    group_by(Team) %>%
    mutate(venue = as.numeric(factor(Venue))) %>%
    ungroup()
  # --- Optional: add candidate features for experimentation -------------------
  # Uncomment to add extra features before selection. Then add their column names
  # to the select() calls below and retrain via source("source_code/retrain_model.R").
  # Use analysis_code/feature_investigation.R to check if they actually help first.
  # WARNING: changing features changes col_num — model must be retrained afterwards.
  # new <- add_extra_features(new)

  # Betless feature set (current-round odds excluded) — drives the betless baseline model
  # (model/model_betless). matchType is kept for downstream joins but dropped from model
  # inputs in prediction_model.R. complete.cases() here does NOT depend on current-round
  # odds, so upcoming fixtures survive even when odds are unavailable.
  future_data_lean <- new %>%
    select(results, Season, team, opposition, status, last_scoreDiff,
           pre_rate, pre_oppRate, last_score_acc,
           matches_won, last_encounter_margin, last_rateDiff, last_Odds,
           last_LineOdds, last_encounter_SC, last_encounter_disposals,
           season_for, season_against, opp_season_for, opp_season_against, venue, matchType
    ) %>%
    filter(complete.cases(.)) %>%
    filter(results == 0 | results == 1 | results == 999)

  # Full feature set including current-round odds — drives the primary model (model/model)
  # when MODEL_BETLESS = FALSE. complete.cases() naturally keeps only matches with odds
  # available, so the primary model is applied to those and the betless baseline covers the rest.
  future_data_full <- new %>%
    select(results, Season, team, opposition, status, last_scoreDiff,
           pre_rate, pre_oppRate, Odds, Opp_Odds, line_Odds, Opp_lineOdds, last_score_acc,
           matches_won, last_encounter_margin, last_rateDiff, last_Odds,
           last_LineOdds, last_encounter_SC, last_encounter_disposals,
           season_for, season_against, opp_season_for, opp_season_against, venue, matchType
    ) %>%
    filter(complete.cases(.)) %>%
    filter(results == 0 | results == 1 | results == 999)

  # Margin prediction dataframe — includes Team/Opposition/matchType for downstream joins
  score_data_lean <- new %>%
    select(Margin, Team, Opposition, Season, team, opposition, status, last_scoreDiff,
           pre_rate, pre_oppRate, last_score_acc,
           matches_won, last_encounter_margin, last_rateDiff, last_Odds,
           last_LineOdds, last_encounter_SC, last_encounter_disposals,
           season_for, season_against, opp_season_for, opp_season_against, venue, matchType
    ) %>%
    filter(complete.cases(.)) %>%
    filter(Margin != 0)

  list(
    future_data_lean = future_data_lean,
    future_data_full = future_data_full,
    score_data_lean  = score_data_lean,
    round            = round,
    glicko_rate      = glicko_rate,
    round.no         = round.no
  )
}

# ---- IO shell: every read/fetch/write, then call the pure core ---------------
build_features <- function(year = as.numeric(format(Sys.Date(), "%Y")),
                           betting_join = NULL) {
  YEAR <- year

  # Fetch full season fixture in canonical form, then auto-detect the
  # current/upcoming round. get_fixture() (functions/get_fixture.R) absorbs the
  # AFL.com source quirks — columns, team-name spellings, UTC timestamps.
  fixture_all <- get_fixture(season = YEAR, source = "AFL")

  round.no <- fixture_all %>%
    filter(as.Date(Date) >= Sys.Date()) %>%
    pull(Round) %>%
    min()

  if (!is.finite(round.no)) stop("No upcoming rounds found in the ", YEAR, " fixture — season may be complete.")

  message("Auto-detected round: Round ", round.no)

  fixture <- fixture_all %>%
    filter(Round == round.no)

  ##########----- Gather Data from fitZroy package -----##########
  # player stats
  dat <- read.csv('csv_files/AFLstats.csv')
  dat <- dat %>% select(!X) %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  options(timeout = 300)
  dat.new <- tryCatch(
    fetch_player_stats_footywire(season = YEAR, check_existing = TRUE, round_number = round.no - 1) %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")),
    error = function(e) {
      warning("Failed to fetch player stats from Footywire: ", conditionMessage(e),
              "\nProceeding with existing data in csv_files/AFLstats.csv.")
      NULL
    }
  )

  if (!is.null(dat.new)) {
    dat <- plyr::rbind.fill(dat, dat.new)
    dat <- dat %>% unique()
    write.csv(dat, file = 'csv_files/AFLstats.csv')
  }

  #read in betting odd .csv
  betting_odds <- read.csv('csv_files/betting_odds.csv')

  ## Get match results
  results <- tryCatch(
    fetch_results_afltables(season = START_SEASON:YEAR),
    error = function(e) stop("Failed to fetch match results from AFLTables: ", conditionMessage(e))
  )

  # Current round odds: prefer the in-memory betting_join (produced by betting_odds.R);
  # fall back to the CSV it writes so odds still load when run in a session where
  # betting_odds.R wasn't sourced (or failed).
  if (is.null(betting_join) && file.exists("csv_files/betting_odds_round.csv")) {
    betting_join <- read.csv("csv_files/betting_odds_round.csv")
    betting_join <- betting_join[, !names(betting_join) %in% "X"]   # drop CSV row-index column
    message("betting_join loaded from csv_files/betting_odds_round.csv")
  }

  build_features_core(
    results      = results,
    dat          = dat,
    betting_odds = betting_odds,
    fixture      = fixture,
    round.no     = round.no,
    betting_join = betting_join
  )
}
