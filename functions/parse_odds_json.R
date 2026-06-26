# parse_odds_json.R — parse the-odds-api v4 JSON into per-match median odds.
#
# One walk of the nested JSON (game -> bookmakers -> market -> outcomes), shared
# by the line (spreads / $point) and h2h ($price) markets. line_odds() and
# h2h_odds() are thin wrappers kept so build_round_odds() (betting_odds.R) is
# unchanged.
#
#   market      — odds-api market key: "spreads" or "h2h"
#   value_field — outcome field to take the median of: "point" or "price"
# Returns: team.a (home), team.b (away), Date, median_team.a, median_team.b, Season
parse_odds_json <- function(betting_json, market, value_field) {
  out <- tibble()
  for (i in betting_json) {              # no $data wrapper in v4
    team.a <- i$home_team
    team.b <- i$away_team
    Date   <- as.POSIXct(i[["commence_time"]], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    vals <- tibble()
    for (j in i[["bookmakers"]]) {        # "bookmakers" not "sites"
      mkt <- Filter(function(m) m$key == market, j$markets)
      if (length(mkt) == 0) next
      outcomes <- mkt[[1]]$outcomes
      oa <- Filter(function(o) o$name == team.a, outcomes)   # match by name, not position
      ob <- Filter(function(o) o$name == team.b, outcomes)
      if (length(oa) == 0 || length(ob) == 0) next
      vals <- rbind(vals, tibble(a = oa[[1]][[value_field]], b = ob[[1]][[value_field]]))
    }
    if (nrow(vals) == 0) next

    med <- vals %>% summarise(median_team.a = median(as.numeric(a)),
                              median_team.b = median(as.numeric(b)))
    out <- rbind(out, tibble(team.a = team.a, team.b = team.b, Date = Date) %>% cbind(med))
  }
  out %>% mutate(Season = format(Date, "%Y"))
}

# spreads (line) odds -> home/away line columns (shape kept for build_round_odds)
line_odds <- function(betting_json) {
  parse_odds_json(betting_json, market = "spreads", value_field = "point") %>%
    mutate(Home.Team      = team.a,
           Away.Team      = team.b,
           Home.Line.Odds = median_team.a,
           Away.Line.Odds = median_team.b) %>%
    select(Date, Home.Team, Away.Team, Home.Line.Odds, Away.Line.Odds, team.a, team.b, Season)
}

# h2h (win) odds -> raw medians
h2h_odds <- function(h2h_json) {
  parse_odds_json(h2h_json, market = "h2h", value_field = "price")
}
