#run simulations on n runs
score_sim <- simulation(runs = 10000) # returns a list of data frames to use below
# summarize findings from simulation
mean_score <- score_sim$score_sim %>%
  group_by(game, team, opp) %>% 
  summarise(rating.mean=mean(value), rating.sd = sd(value), rating.median = median(value))
# add some additional variables 
mean_score <- mean_score %>% 
  mutate(result = ifelse(rating.mean < 0, opp, team)) %>% 
  mutate(match = paste(team, opp, sep = " v "))
games = nrow(round) # matches for the round

#create custom colors for teams
cols <- (c("Gold Coast" = 'gold',"GWS" = 'orange',"Collingwood" = 'black', "North Melbourne" = 'mediumblue',"Sydney"= "firebrick1", "Fremantle" ="purple","Port Adelaide" = "lightseagreen", "Adelaide" = "gold",
           "West Coast" = "blue", "Melbourne" = "darkblue", "Western Bulldogs" = "ivory1", "Richmond" = "yellow","Carlton" = "navy","Hawthorn" = "chocolate4", "St Kilda" = "grey", "Essendon" =  "red3",
           "Brisbane" = "maroon", "Geelong" = "dodgerblue"))

margin_data <- score_sim$score_data_lean %>%
  select(Margin,margin_est_linear, margin_est_rand, pred_win_prob, line_Odds, Odds, status, last_encounter_SC, pred_cat, pred_cat_factor)


#computation of the standard error of the mean
sim_summary <- score_sim$score_sim %>% 
  group_by(match) %>% 
  summarise(mean = mean(value), sd = sd(value), cv = sd/mean, sem = sd(value)/sqrt(length(value)), lower_CI = mean(value)-2*sem, upper_CI = mean(value)+2*sem)

score_data_lean_error<-score_sim$score_data_lean %>%
  filter(Margin != 999) %>% 
  mutate(margin_linear_error = Margin - margin_est_linear,
         margin_rand_error = Margin - margin_est_rand)

