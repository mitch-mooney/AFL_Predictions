library(ggpubr)
library(reactable)
library(htmltools)
library(sparkline)

source('functions/reactable_function.R')

#bind guess with fixture
prob_pred_df <- prob_pred_df%>%
  rename(Loss_prob = V1,
         Win_Prob = V2,
         #Draw_Prob = V3,
         Tips = V3)

new_predictions<-score_sim$score_data_lean %>% 
  filter(Margin == 999) %>% 
  mutate(Tips = ifelse(pred_win_prob > 0.5, 1, 0)) %>% 
  select(Tips,	pred_loss_prob,	pred_win_prob,margin_est_linear,margin_est_rand)

table<-cbind(round, new_predictions)
table$Margin <- NULL

table %<>% 
  mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition))
  
table %<>%
  select(Date,Match_id,	Season,	Team,	Opposition,	Status,	Venue,	Round,	results,	Odds,	line_Odds,	Opp_Odds,	Opp_lineOdds,	Tips,	pred_loss_prob,	pred_win_prob, Team_predicted,	margin_est_linear,	margin_est_rand) %>% 
  rename(
    Loss_prob = pred_loss_prob,
    Win_Prob = pred_win_prob,
    margin_estimate_1 = margin_est_linear,
    margin_estimate_2 = margin_est_rand
  )


table_final <- table %>% 
  filter(Status == "Home")

table_final %<>% 
  select(Date, Season, Team, Opposition, Venue, Round, Loss_prob, Win_Prob, Team_predicted, margin_estimate_1) %>% 
  mutate(Loss_prob = round(Loss_prob, digits = 2))%>% 
  mutate(Win_Prob = round(Win_Prob, digits = 2))

# generate table to merge with simulation plot
t <- table_final %>% 
  select(Team, Opposition, Round, Loss_prob, Win_Prob, margin_estimate_1, Team_predicted) %>% 
  rename(Pred_Winner=Team_predicted,Pred_Margin = margin_estimate_1) %>% 
  mutate(Pred_Margin = round(Pred_Margin,0))

current_round <- unique(t$Round)
season_pred <- read.csv('csv_files/round_predictions.csv')
season_pred %<>% filter(!Round %in% current_round) %>% select("Team","Opposition","Round","Loss_prob","Win_Prob","Pred_Margin","Pred_Winner")
season_pred.new <- rbind(season_pred, t)
write.csv(season_pred.new, 'csv_files/round_predictions.csv')


# use reactable to use team logos
options(reactable.theme = reactableTheme(
  color = "hsl(233, 9%, 87%)",
  backgroundColor = "hsl(233, 9%, 19%)",
  borderColor = "hsl(233, 9%, 22%)",
  stripedColor = "hsl(233, 12%, 22%)",
  highlightColor = "hsl(233, 12%, 24%)",
  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

reactable(t, columns = list(
  Team = colDef(maxWidth = 150, align = "center", cell = function(value) {
    img_src <- knitr::image_uri(sprintf("images/%s.png", value))
    image <- img(src = img_src, height = "60px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "80px"), image)
    )
    
  }),
  Opposition = colDef(maxWidth = 150, align = "center", cell = function(value) {
    img_src <- knitr::image_uri(sprintf("images/%s.png", value))
    image <- img(src = img_src, height = "60px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "80px"), image)
    )
  }),
  Pred_Winner = colDef(name = "Predicted Winner", maxWidth = 150, align = "center", cell = function(value) {
    img_src <- knitr::image_uri(sprintf("images/%s.png", value))
    image <- img(src = img_src, height = "60px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "80px"), image)
    )
  }),
  Round = colDef(align = "center", maxWidth = 120),
  Loss_prob = colDef(name = "Loss Probability", align = "center", maxWidth = 120),
  Win_Prob = colDef(name = "Win Probability", align = "center", maxWidth = 120),
  Pred_Margin = colDef(name = "Predicted Margin", align = "center", maxWidth = 120, format = colFormat(digits = 0))
))


# simulation plot
# create plot for simulation
score_sim$score_sim %>% 
  mutate(result = ifelse(value < 0, opp, team)) %>% 
  filter(game < (games/2 +1)) %>% #change this to suit how many matches there are that round
  ggplot(aes(x = value, color = result))+
  geom_histogram(binwidth = 1,  alpha = 0.8)+
  geom_vline(data=mean_score[1:(games/2),], aes(xintercept=rating.mean,  colour=result), #change mean_score[1:games/2]
             linetype="dashed", size=1)+
  scale_colour_manual(values = cols)+
  labs(title = paste("Match simulation of AFL:", score_sim$score_sim$round,sep = " "),
       color = "Team",
       x = "simulated margin")+
  scale_x_continuous(breaks = seq(-100, 100, 20))+
  theme_AFL(base_size = 12)+
  facet_grid(game+match~.,labeller = label_wrap_gen(width = 0.5, multi_line = TRUE))

# CDF plot of the simulations
score_sim$score_sim %>% 
  mutate(result = ifelse(value < 0, opp, team)) %>% 
  filter(game < (games/2 +1)) %>% #change this to suit how many matches there are that round
  ggplot(aes(x = value, color = team))+
  stat_ecdf(size = 1.25)+
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.9, color = '#498181')+
  geom_hline(yintercept = 0.5, size = 1.5,alpha = 0.9, color = '#498181')+
  scale_colour_manual(values = cols)+
  labs(title = "Simulated cummulative density function",
       x = "Home Team Margin",
       y = "cummulative density")+
  xlim(-75,75) +
  # Add images to end point of line graph per team
  theme_AFL(base_size = 8, background_hex = '#64B2B2')+
  theme(legend.position = 'None')+
  facet_grid(~game+match,labeller = label_wrap_gen(width = 0.5, multi_line = TRUE))


### Ratings update  ###
matches<-results %>% 
  filter(Season >= 2010) %>% 
  group_by(Season, Round.Number) %>% 
  summarise_each(funs(n_distinct(Date))) %>% 
  select(Season, Round.Number, Date)

lag <-tail(matches$Date, 1)
round_num <- tail(matches$Round.Number, 1)

#glicko ratings table
rating_history <- glicko %>% 
  filter(var == "Rating") %>% 
  group_by(match) %>% 
  mutate(rank = rank(-value))%>% 
  group_by(Team) %>% 
  mutate(lag = lag(rank, n =lag)) %>% 
  ungroup()%>% 
  mutate(change = ifelse(rank < lag, "Up", ifelse(rank > lag, "Down", "Unchanged")),
         match = as.numeric(match)) %>% 
  filter(match == max(match)) %>% 
  select(Team, change) %>%
  rename(Player = Team)

team_rate <- glicko_rate$ratings
team_rate %<>% 
  select(!c(Lag, Deviation, Volatility)) %>% 
  mutate(Rating = round(Rating, 0), Rank = rank(-Rating)) %>% 
  select(Rank, Player, Rating)

team_rate<-left_join(rating_history, team_rate, by = c("Player"))

spark_table <- glicko_clean %>%
  group_by(Team) %>%
  summarise(Rating = round(tail(value, n = 1), 0),sparkline = list(tail(value, n = round_num))) %>% 
  rename(Player = Team)

spark_table <- merge(team_rate,spark_table, by=c("Rating", "Player"), all.x=TRUE, all.y=TRUE)

reactable_function(data = spark_table)
