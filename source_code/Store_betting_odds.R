## Get match results
library(tidyverse)
library(fitzRoy)
library(data.table)
library(PlayerRatings)
library(plotly)
library(lubridate)
library(reshape2)
library(ggpmisc)
library(magrittr)

results<-fetch_results_afltables(season = YEAR)

newest.results <- results %>% 
  filter(Season == YEAR, Round.Number == round.no+1) %>% 
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

betting %<>% 
  select(!X) %>% 
  unique() %>% 
  drop_na(Date)

betting_csv <- rbind(betting, newest.betting) %>% 
  drop_na(Date)

write.csv(betting_csv, "csv_files/betting_odds.csv")

