library(tidyverse)

file.source = list.files("functions/")
sapply(paste0('functions/', file.source), source)
# Get recent betting odds
odds_api_key <- Sys.getenv("ODDS_API_KEY")
if (odds_api_key == "") stop("ODDS_API_KEY environment variable not set. Add it to ~/.Renviron")
odds_base <- paste0("https://api.the-odds-api.com/v4/sports/aussierules_afl/odds/?apiKey=", odds_api_key, "&regions=au&markets=")
line.odds_json <- jsonlite::read_json(paste0(odds_base, "spreads"))
h2h.odds_json  <- jsonlite::read_json(paste0(odds_base, "h2h"))
if (length(line.odds_json) == 0) stop("Odds API returned empty spreads response — check API key/quota")
if (length(h2h.odds_json)  == 0) stop("Odds API returned empty h2h response — check API key/quota")
# gather median line odds
line.median<-line_odds(line.odds_json)
# gather median h2h odds
h2h.median <- h2h_odds(h2h.odds_json)
# join and clean line and h2h
betting.df <- line.median %>%
  left_join(h2h.median, by = c("team.a", "team.b", "Date", "Season")) %>%
  mutate(Home.Win.Odds = ifelse(team.a == Home.Team, median_team.a, median_team.b),
         Away.Win.Odds = ifelse(team.b == Away.Team, median_team.b, median_team.a),
         Home.Team = expand_odds_team_names(sapply(strsplit(Home.Team, " "), `[`, 1)),
         Away.Team = expand_odds_team_names(sapply(strsplit(Away.Team, " "), `[`, 1))) %>%
  select(Date, Home.Team, Away.Team, Home.Win.Odds, Away.Win.Odds, Home.Line.Odds, Away.Line.Odds)

betting_idx <- rep(1:nrow(betting.df), 2)
betting_df <- betting.df[betting_idx,]

betting_join<-betting_df%>%
  group_by(Date,Home.Team)%>%
  mutate(num = rep(1:2),
         Status = ifelse(num == 1, "Home", "Away"),
         Team = ifelse(num == 1, Home.Team, Away.Team),
         Opposition = ifelse(num == 1, Away.Team, Home.Team),
         Odds = ifelse(num == 1, Home.Win.Odds, Away.Win.Odds),
         line_Odds = ifelse(num==1, Home.Line.Odds, Away.Line.Odds),
         Opp_Odds = ifelse(num==1, Away.Win.Odds, Home.Win.Odds),
         Opp_lineOdds = ifelse(num==1, Away.Line.Odds, Home.Line.Odds)) %>% 
  ungroup() %>% 
  select(Team, Opposition, Status, Odds, line_Odds, Opp_Odds, Opp_lineOdds)


