AFL <- fitzRoy::fetch_player_stats_fryzigg(season = 1897:2021)



AFL <- AFL %>% select(player_id, player_first_name, player_last_name, player_team, match_local_time, match_id, goals)


# function to calculate hindex
h_index = function(cites) {
  
  if(max(cites) == 0) return(0) # assuming this is reasonable
  
  cites = cites[order(cites, decreasing = TRUE)]
  
  tail(which(cites >= seq_along(cites)), 1)
  
}



# calculate h index and

AFL %>%
  
  group_by(player_id) %>%
  
  summarise(Player_Name = paste(player_first_name, player_last_name, sep = " "), Team = player_team, hindex = h_index(goals)) %>%
  
  distinct(Player_Name, hindex) %>%
  
  ungroup() %>% 
  
  arrange(-hindex) %>%
  
  select(-player_id) %>%
  
  reactable::reactable(defaultColDef = reactable::colDef(
    
    # default table settings
    
    header = function(value) gsub("_", " ", value, fixed = TRUE),
    
    align = "center",
    
    minWidth = 70,
    
    headerStyle = list(background = "#B0F1E4")),
    
    highlight = TRUE,
    
    pagination = FALSE)