# loop through json creating a data frame and bind together
h2h_odds <- function(h2h_json){
  line_data <- tibble()
  for (i in h2h_json){  # no $data wrapper in v4
    team.a <- i$home_team
    team.b <- i$away_team
    Date   <- as.POSIXct(i[["commence_time"]], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    line.df <- tibble()
    for (j in i[["bookmakers"]]){
      h2h_market <- Filter(function(m) m$key == "h2h", j$markets)
      if (length(h2h_market) == 0) next
      outcomes <- h2h_market[[1]]$outcomes
      odds_a <- Filter(function(o) o$name == team.a, outcomes)
      odds_b <- Filter(function(o) o$name == team.b, outcomes)
      if (length(odds_a) == 0 || length(odds_b) == 0) next
      row <- tibble(team.a_odds = odds_a[[1]]$price, team.b_odds = odds_b[[1]]$price)
      line.df <- rbind(line.df, row)
    }
    if (nrow(line.df) == 0) next

    line_sum <- line.df %>% summarise(median_team.a = median(team.a_odds),
                                      median_team.b = median(team.b_odds))
    teams     <- tibble(team.a = team.a, team.b = team.b, Date = Date) %>% cbind(line_sum)
    line_data <- rbind(line_data, teams)
  }
  line_data <- line_data %>% mutate(Season = format(Date, "%Y"))
  return(line_data)
}
