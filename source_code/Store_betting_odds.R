newest.results <- results %>% 
  filter(Season == 2021, Round.Number == 3) %>% 
  select(Date, Venue, Round.Number, Home.Team, Away.Team, Home.Points, Away.Points)

newest.betting <- betting_join %>% 
  filter(Status == "Home") %>% 
  select(!Status) %>% 
  rename(Home.Team = Team,
         Away.Team = Opposition) %>% 
  mutate(Home.Team = ifelse(Home.Team == "Western Bulldogs", "Footscray", 
                            ifelse(Home.Team == "Brisbane", "Brisbane Lions", Home.Team)),
         Away.Team = ifelse(Away.Team == "Western Bulldogs", "Footscray", 
                            ifelse(Away.Team == "Brisbane", "Brisbane Lions", Away.Team))) %>% 
  left_join(newest.results, by = c("Home.Team", "Away.Team"))

newest.betting %<>% 
  mutate(Home.Margin = Home.Points - Away.Points,
         Away.Margin = Away.Points - Home.Points,
         Season = as.integer(format(Date, "%Y")),
         Date = as.character.Date(Date, format = "%d/%m/%Y")) %>% 
  rename(Round = Round.Number,
         Home.Score = Home.Points,
         Away.Score = Away.Points,
         Home.Win.Odds = Odds,
         Away.Win.Odds = Opp_Odds,
         Home.Line.Odds = line_Odds,
         Away.Line.Odds = Opp_lineOdds) %>% 
  select("Date", "Venue", "Home.Team", "Away.Team","Home.Score","Away.Score", "Home.Margin", "Away.Margin","Home.Win.Odds", "Away.Win.Odds", "Home.Line.Odds", "Away.Line.Odds", "Round", "Season")

betting_csv <- rbind(betting, newest.betting)

write.csv(betting_csv, "csv_files/betting_odds.csv")
