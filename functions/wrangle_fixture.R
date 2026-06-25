wrangle_fixture <- function(round = 1, data = fixture){
  fixture_round <- data %>%
    filter(Round == round)

  fixture_join <- fixture_round %>%
    expand_home_away("Season.Game") %>%
    mutate(Round = paste("Round", Round, sep = " "),
           results = 999,
           Margin = 999,
           Match_id = rep(1:(0+n()/2), times=2, each=1)) %>%
    select(Date, Match_id, Season, Team, Opposition, Status, Venue, Round, results, Margin)

  fixture_join$Team       <- normalize_team_names(fixture_join$Team)
  fixture_join$Opposition <- normalize_team_names(fixture_join$Opposition)

  return(fixture_join)
}
