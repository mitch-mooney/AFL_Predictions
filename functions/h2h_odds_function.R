# loop through json creating a data frame and bind together
h2h_odds <- function(h2h_json){
  bet_data <- h2h_json$data
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
      points <- j[["odds"]][["h2h"]][1:2]
      names(points) <- c("team.a_odds", "team.b_odds")
      points <- dplyr::bind_rows(points)
      points$site <- site
      
      line.df <- rbind(line.df, points)
      
    }
    
    line_sum <- line.df %>% summarise(median_team.a = median(team.a_odds), median_team.b = median(team.b_odds))
    
    teams <- cbind(teams, line_sum)
    
    # identify home team and away team

    line_data <- rbind(line_data, teams)
  }
  line_data <- line_data %>% mutate(Season = format(Date, "%Y"))
  return(line_data)
}
