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

#new_predictions<-score_sim$score_data_lean %>% 
new_predictions<-future_data_lean %>% 
  left_join(
    score_data_lean %>% 
      select(Team, Opposition, Margin, Season, team, opposition, status, venue, matchType)
  ) %>% 
  filter(Margin == 999) %>% 
  mutate(Tips = ifelse(pred_win_prob > 0.5, 1, 0)) %>% 
  select(Team, Opposition, Tips,	pred_loss_prob,	pred_win_prob,margin_est_linear)

table <- round %>% 
  left_join(
    new_predictions,
    by = c("Team", "Opposition")
  ) 

#table<-cbind(round, new_predictions)
table$Margin <- NULL

table %<>% 
  mutate(Team_predicted = ifelse(Tips == 1, Team, Opposition))
  
table %<>%
  select(Date,Match_id,	Season,	Team,	Opposition,	Status,	Venue,	Round,	results,	Odds,	line_Odds,	Opp_Odds,	Opp_lineOdds,	Tips,	pred_loss_prob,	pred_win_prob, Team_predicted,	margin_est_linear) %>% 
  rename(
    Loss_prob = pred_loss_prob,
    Win_Prob = pred_win_prob,
    margin_estimate_1 = margin_est_linear
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
  mutate(Pred_Margin = round(Pred_Margin,0),
         Pred_Margin = ifelse(Win_Prob >=0.5 & Pred_Margin < 0, 0,
                              ifelse(Win_Prob <= 0.5 & Pred_Margin > 0, 0, Pred_Margin)),
         Season = as.numeric(format(Sys.Date(), "%Y")))

current_round <- unique(t$Round)
current_season <- as.integer(unique(t$Season))

season_pred <- read.csv('csv_files/round_predictions.csv')
season_pred <- season_pred[!(season_pred$Season == current_season & season_pred$Round == current_round),c("Team","Opposition","Round","Loss_prob","Win_Prob","Pred_Margin","Pred_Winner", "Season")] 

season_pred.new <- rbind(season_pred, t)
write.csv(season_pred.new, 'csv_files/round_predictions.csv')


# Add team rating csv — append to history, overriding existing entry for this round
new_ratings <- data.frame(
  Season = current_season,
  Team   = glicko_rate$ratings$Player,
  Round  = round.no - 1,
  rating = glicko_rate$ratings$Rating
)

existing_ratings <- read.csv('csv_files/Team_Ratings.csv')
existing_ratings <- existing_ratings[!(existing_ratings$Season == current_season &
                                         existing_ratings$Round == round.no - 1), ]
rbind(existing_ratings, new_ratings) %>%
  write.csv('csv_files/Team_Ratings.csv', row.names = FALSE)

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

predictions_table <- reactable(t, columns = list(
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

print(htmltools::browsable(predictions_table))
