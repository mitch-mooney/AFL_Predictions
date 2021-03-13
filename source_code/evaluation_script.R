
#bits and brier scores following https://rpubs.com/DamienG/613310
season_predictions$predicted_prob = pmax(season_predictions$Loss_prob, season_predictions$Win_Prob)
season_predictions$brier = (season_predictions$predicted_prob - season_predictions$Tip_Outcome)^2
season_predictions$bits = ifelse(season_predictions$Tip_Outcome == 1, 1 + log(season_predictions$predicted_prob, base = 2), 
                                 ifelse(season_predictions$Tip_Outcome == 0, 1 + log(1 - season_predictions$predicted_prob, base = 2),
                                        1 + 0.5*log(season_predictions$predicted_prob*(1-season_predictions$predicted_prob), base = 2)))

#create dataframe comparing home and away prediction scores
accuracy <- season_predictions %>% 
  group_by(Status) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), 
            Bits = round(sum(bits), 3), 
            MAE = round(mean(margin_error),2),
            Accuracy = round(100*mean(Tip_Outcome), 1), 
            Tips = sum(correct_tips))

formattable::formattable(accuracy, align = c("l", rep("c", NCOL(accuracy) - 1)))

model_accuracy <- season_predictions %>%
  mutate(Round = sapply(strsplit(Round," "), `[`, 2),
         Round = as.numeric(Round)) %>% 
  group_by(Status, Round) %>%
  filter(Status == "Home") %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         margin_error = abs(margin_estimate_1 - Actual.Margin)) %>% 
  summarise(Brier = round(mean(brier), 3), 
            Bits = round(mean(bits), 3), 
            MAE = round(mean(margin_error),2),
            Tips = sum(correct_tips), 
            Matches = n(), 
            `Round Accuracy` = round(Tips/n(), 2)) %>%
  mutate(Cumulative = cumsum(Tips), 
         `Cumulative Matches` = cumsum(Matches), 
         `Cumulative Accuracy` = round((Cumulative/`Cumulative Matches`),2)) %>% 
  select(Round, Matches, `Cumulative Matches`,Bits, MAE, Tips, Cumulative, `Round Accuracy`, `Cumulative Accuracy`)

formattable::formattable(model_accuracy, align = c("l", rep("c", NCOL(accuracy))))
# see if how well predicting teams
team_accuracy <- season_predictions %>% 
  group_by(Status, Team) %>% 
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         margin_error = margin_estimate_1 - Actual.Margin) %>% 
  summarise(Brier = round(mean(brier), 3), 
            Bits = round(mean(bits), 3), 
            MAE = round(mean(margin_error)),
            Accuracy = round(100*mean(Tip_Outcome), 1))
ormattable::formattable(team_accuracy, align = c("l", rep("c", NCOL(team_accuracy) - 1)))

# Betting return
ROI<-season_predictions %>%
  mutate(correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         paid = ifelse(Actual == 1, Odds, Opp_Odds),
         return = Tip_Outcome * paid,
         ROI = return *10)

ROI %>% 
  filter(predicted_prob > 0.50, Round != "Round 2") %>% 
  group_by(Status, Round) %>% 
  summarise(ROI = sum(ROI), 
            Investment = n()*10)

ROI %>% 
  filter(predicted_prob > 0.50,
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>%
  group_by(Status) %>% 
  summarise(ROI = sum(ROI), 
            Investment = n()*10)

favorites <- season_predictions %>% 
  mutate(favorite = ifelse(Odds < Opp_Odds, "Favorite", "Outside"),
         correct_tips = ifelse(Tip_Outcome == 1, 1, 0),
         paid = ifelse(Actual == 1, Odds, Opp_Odds),
         return = Tip_Outcome * paid,
         ROI = return *10)

favorites %<>%
  mutate(pred_cat = ifelse(predicted_prob < 0.1, 1, 
                           ifelse(predicted_prob > 0.1 & predicted_prob < 0.2, 2,
                                  ifelse(predicted_prob > 0.2 &predicted_prob < 0.3, 3,
                                         ifelse(predicted_prob > 0.3 & predicted_prob < 0.4, 4,
                                                ifelse(predicted_prob > 0.4 & predicted_prob < 0.5, 5,
                                                       ifelse(predicted_prob > 0.5 & predicted_prob < 0.6, 6,
                                                              ifelse(predicted_prob > 0.6 & predicted_prob< 0.7, 7,
                                                                     ifelse(predicted_prob > 0.7 & predicted_prob < 0.8, 8,
                                                                            ifelse(predicted_prob > 0.8 & predicted_prob < 0.9, 9, 10))))))))))

favorites %>% 
  filter(predicted_prob > 0.5,
         Status == "Away",
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(favorite, Round) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)%>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100)

favorites %>% 
  filter(predicted_prob > 0.5,
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(favorite, Status) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10) %>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100)

favorites %>% 
  filter(predicted_prob > 0.50, 
         Status == "Away",
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(pred_cat, favorite) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)%>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100) %>% 
  ggplot(aes(x = pred_cat, y = per_return, color = favorite))+
  geom_line()

favorites %>% 
  filter(predicted_prob > 0.50,
         Round != "Round 2", Round != "Round 3",Round != "Round 4") %>% 
  group_by(pred_cat, Status) %>% 
  summarise(ROI = sum(ROI), Investment = n()*10)%>% 
  mutate(profit = ROI - Investment, 
         per_return = (profit/Investment)*100) %>% 
  ggplot(aes(x = pred_cat, y = per_return, color = Status))+
  geom_line()
