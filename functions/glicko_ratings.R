# glicko_ratings.R — compute Glicko2 ratings from match results.
#
# Hides the glicko2() call and the fragile ratings-history reshape (melt / cbind /
# strsplit / apply(!=0) row filter) behind a small interface. Returns:
#   rate  — the glicko2 object ($ratings, $history); a build_features output
#   clean — per-team cleaned ratings: Team, match_num, value, rate_change
#
# `match` must carry Date, Status, Team, Opposition, results (results coded
# 0/1/2; 2 = draw, mapped to 0.5 for Glicko).
#
# rank(date) gives one Glicko rating period per calendar date (all that date's
# games rated simultaneously) — the right granularity for AFL's round structure,
# so it is kept as-is.
#
# Post-game ratings are selected via `played` — the (team, period) pairs the team
# actually appeared in — rather than the old `apply(glicko != 0)` filter, which
# used "rating changed" as a proxy for "played" and would silently drop (and
# misalign) any game that produced exactly zero rating change.
glicko_ratings <- function(match) {
  ratings <- match %>%
    filter(Status == 'Home') %>%
    select(Date, Team, Opposition, results)%>%
    mutate(results = ifelse(results == 2, 0.5, results)) %>%
    arrange(Date)

  ratings$date <- as.integer(format(ratings$Date, "%Y%m%d"))
  ratings$match <- rank(ratings$date)

  # period = dense_rank(date) matches glicko2's sequential history period labels
  # (1..n_dates); `played` is every (team, period) pair, home and away.
  ratings$period <- dplyr::dense_rank(ratings$date)
  played <- dplyr::bind_rows(
    dplyr::transmute(ratings, period, Team = Team),
    dplyr::transmute(ratings, period, Team = Opposition)
  ) %>%
    dplyr::distinct()

  ratings %<>%
    select(match, Team, Opposition, results)

  ratings$Match_id<-NULL

  glicko_rate<-glicko2(ratings, history = T)

  #make dataframe with history ratings
  glicko <- as.data.frame(glicko_rate$history)
  setDT(glicko, keep.rownames = TRUE)[]
  glicko <- melt(glicko)
  glicko$variable <- as.character(glicko$variable)
  var <-data.frame(do.call('rbind', strsplit(as.character(glicko$variable),'.',fixed=TRUE)))
  glicko<-cbind(glicko, var)
  names(glicko)[1] <- "Team"
  names(glicko)[4] <- "match"
  names(glicko)[5] <- "var"
  glicko %<>%
    filter(var == "Rating")
  #rate$match_num <- with(rate, match(match, unique(Date)))

  ## See Glicko Prediction.R for ratings predictions ##

  #prepare data for merging with player stats
  glicko %<>%
    group_by(Team) %>%
    mutate(rate_change = (value) - lag(value),
           rate_change = ifelse(is.na(rate_change), 2200 - value, rate_change)) %>%
    ungroup()

  # keep only (Team, period) rows where the team actually played that period
  glicko$match <- as.integer(glicko$match)
  keep <- paste(glicko$Team, glicko$match) %in% paste(played$Team, played$period)
  glicko_clean <- glicko[keep, ]

  glicko_clean %<>%
    group_by(Team) %>%
    mutate(match_num = order(order(match, decreasing=F))) %>%
    select(Team, match_num, value, rate_change) %>%
    ungroup()

  list(rate = glicko_rate, clean = glicko_clean)
}
