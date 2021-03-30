wrangle_fixture <- function(round = 1, data = fixture){
  fixture_round <- fixture %>% 
    filter(Round == round)
  
  fixture_idx <- rep(1:nrow(fixture_round), 2)
  fixture_df <- fixture_round[fixture_idx,]
  
  fixture_join<-fixture_df%>%
    group_by(Season.Game)%>%
    mutate(num = row_number(),
           Status = ifelse(num == 1, "Home", "Away"),
           Team = ifelse(num == 1, Home.Team, Away.Team),
           Opposition = ifelse(num == 1, Away.Team, Home.Team),
           Round = paste("Round", Round, sep = " "),
           results = 999,
           Margin = 999) %>% 
    ungroup() %>% 
    mutate(Match_id = rep(1:(0+nrow(fixture_df)/2), times=2, each=1)) %>% 
    select(Date, Match_id, Season, Team, Opposition, Status,Venue, Round, results, Margin)
  
  fixture_join$Team<-stringr::str_replace(fixture_join$Team, "Brisbane Lions$", "Brisbane")
  fixture_join$Opposition<-stringr::str_replace(fixture_join$Opposition, "Brisbane Lions$", "Brisbane")
  
  fixture_join$Team<-stringr::str_replace(fixture_join$Team, "Footscray$", "Western Bulldogs")
  fixture_join$Opposition<-stringr::str_replace(fixture_join$Opposition, "Footscray$", "Western Bulldogs")
  
  return(fixture_join)
}
