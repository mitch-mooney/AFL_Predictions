# loop through json creating a data frame and bind together
line_odds <- function(betting_json){
  bet_data <- betting_json$data
  line_data <- tibble()
  for (i in bet_data){
    # find teams
    teams <- i$teams
    names(teams) <- c("team.a", "team.b")
    teams <- dplyr::bind_rows(teams)
    
    # Commence date time
    Date <- as.POSIXct(i[["commence_time"]],
                       origin = "1970-01-1",
                       tz="UTC")
    #bind datetime with teams
    teams <- cbind(teams, Date)
    
    # find odds
    odds <- i[["sites"]]
    line.df <- tibble()
    # loop through the sites and extract line
    for (j in odds){
      site <- j[["site_nice"]]
      points <- j[["odds"]][["spreads"]][["points"]]
      names(points) <- c("team.a_line", "team.b_line")
      points <- dplyr::bind_rows(points)
      points$site <- site
      
      line.df <- rbind(line.df, points)
      
    }
    
    line_sum <- line.df %>% summarise(median_team.a = median(as.numeric(team.a_line)), median_team.b = median(as.numeric(team.b_line)))
    
    teams <- cbind(teams, line_sum)
    
    # identify home team and away team
    Home.Team <- i$home_team
    teams <- teams %>% 
      dplyr::mutate(Home.Team = ifelse(team.a == Home.Team, team.a, team.b),
                    Away.Team = ifelse(team.a == Home.Team, team.b, team.a),
                    Home.Line.Odds = ifelse(team.a == Home.Team, median_team.a, median_team.b),
                    Away.Line.Odds = ifelse(team.a == Home.Team, median_team.b, median_team.a)) %>% 
      select(Date, Home.Team, Away.Team, Home.Line.Odds,Away.Line.Odds,team.a,team.b)
    line_data <- rbind(line_data, teams)
  }
  line_data <- line_data %>% mutate(Season = format(Date, "%Y"))
  return(line_data)
}


