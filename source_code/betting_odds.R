library(tidyverse)

file.source = list.files("functions/")
sapply(paste0('functions/', file.source), source)
# Get recent betting odds
line.odds_json <- jsonlite::read_json("https://api.the-odds-api.com/v3/odds/?apiKey=6eaafecb2712c78a63600da486d7be3b&sport=aussierules_afl&region=au&mkt=spreads")
h2h.odds_json <- jsonlite::read_json("https://api.the-odds-api.com/v3/odds/?apiKey=6eaafecb2712c78a63600da486d7be3b&sport=aussierules_afl&region=au&mkt=h2h")
# gather median line odds
line.median<-line_odds(line.odds_json)
# gather median h2h odds
h2h.median <- h2h_odds(h2h.odds_json)
# join and clean line and h2h
betting <- line.median %>% 
  left_join(h2h.median, by = c("team.a", "team.b", "Date", "Season")) %>% 
  mutate(Home.Win.Odds = ifelse(team.a == Home.Team, median_team.a, median_team.b),
         Away.Win.Odds = ifelse(team.b == Away.Team, median_team.b, median_team.a),
         Home.Team = sapply(strsplit(Home.Team," "), `[`, 1),
         Away.Team = sapply(strsplit(Away.Team," "), `[`, 1)) %>% 
  select(Date, Home.Team, Away.Team, Home.Win.Odds, Away.Win.Odds,Home.Line.Odds, Away.Line.Odds)

#clean up team names
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "Port$", "Port Adelaide")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "Port$", "Port Adelaide")
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "St$", "St Kilda")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "St$", "St Kilda")
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "Gold$", "Gold Coast")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "Gold$", "Gold Coast")
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "Western$", "Western Bulldogs")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "Western$", "Western Bulldogs")
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "North$", "North Melbourne")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "North$", "North Melbourne")
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "West$", "West Coast")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "West$", "West Coast")
betting$Home.Team<-stringr::str_replace(betting$Home.Team, "Greater", "GWS")
betting$Away.Team<-stringr::str_replace(betting$Away.Team, "Greater", "GWS")

betting_idx <- rep(1:nrow(betting), 2)
betting_df <- betting[betting_idx,]

betting_join<-betting_df%>%
  group_by(Date)%>%
  mutate(num = row_number(),
         Status = ifelse(num == 1, "Home", "Away"),
         Team = ifelse(num == 1, Home.Team, Away.Team),
         Opposition = ifelse(num == 1, Away.Team, Home.Team),
         Odds = ifelse(num == 1, Home.Win.Odds, Away.Win.Odds),
         line_Odds = ifelse(num==1, Home.Line.Odds, Away.Line.Odds),
         Opp_Odds = ifelse(num==1, Away.Win.Odds, Home.Win.Odds),
         Opp_lineOdds = ifelse(num==1, Away.Line.Odds, Home.Line.Odds)) %>% 
  ungroup() %>% 
  select(Team, Opposition, Status, Odds, line_Odds, Opp_Odds, Opp_lineOdds)


