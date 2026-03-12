# loop through json creating a data frame and bind together
line_odds <- function(betting_json){
  line_data <- tibble()
  for (i in betting_json){  # no $data wrapper in v4
    home_team <- i$home_team
    away_team <- i$away_team
    team.a    <- home_team
    team.b    <- away_team

    Date <- as.POSIXct(i[["commence_time"]], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    line.df <- tibble()
    for (j in i[["bookmakers"]]){            # "bookmakers" not "sites"
      site <- j[["title"]]                   # "title" not "site_nice"
      spreads_market <- Filter(function(m) m$key == "spreads", j$markets)
      if (length(spreads_market) == 0) next
      outcomes <- spreads_market[[1]]$outcomes
      # match by team name, not position
      pt_a <- Filter(function(o) o$name == team.a, outcomes)
      pt_b <- Filter(function(o) o$name == team.b, outcomes)
      if (length(pt_a) == 0 || length(pt_b) == 0) next
      row <- tibble(team.a_line = pt_a[[1]]$point, team.b_line = pt_b[[1]]$point, site = site)
      line.df <- rbind(line.df, row)
    }
    if (nrow(line.df) == 0) next

    line_sum <- line.df %>% summarise(median_team.a = median(as.numeric(team.a_line)),
                                      median_team.b = median(as.numeric(team.b_line)))
    teams <- tibble(team.a = team.a, team.b = team.b, Date = Date) %>%
      cbind(line_sum) %>%
      mutate(Home.Team      = home_team,
             Away.Team      = away_team,
             Home.Line.Odds = ifelse(team.a == home_team, median_team.a, median_team.b),
             Away.Line.Odds = ifelse(team.a == home_team, median_team.b, median_team.a)) %>%
      select(Date, Home.Team, Away.Team, Home.Line.Odds, Away.Line.Odds, team.a, team.b)

    line_data <- rbind(line_data, teams)
  }
  line_data <- line_data %>% mutate(Season = format(Date, "%Y"))
  return(line_data)
}
