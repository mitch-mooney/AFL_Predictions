library(keras)

data <- future_data_lean
col_num<-as.numeric(ncol(data))
data[1:col_num] <- lapply(data[1:col_num], as.numeric) #make sure all variables are numeric

# returns a list of matrix used for running model
model.data <- model_data(data)
# trains the model using the data frames you choose from the list target needs to be categorical
#model <- model_training(inputs = model.data$full_data_matrix, target = model.data$full_data_target)

#model %>% save_model_tf("model/model") # save model with betting data trained
model <- load_model_tf("model/model") # load model with  betting data trained
#model <- load_model_tf("model/model_betless") # load model with betting data excluded
#evaluate model from test dataset
model %>% 
  evaluate(model.data$test, model.data$testLabels)
# look at the model prediction probabilities
prob<- model %>% 
  predict(model.data$test)
#predict test data targets
#pred <- model %>% 
#  predict_classes(model.data$test)

pred <- model %>% 
  predict(model.data$test) %>% 
  k_argmax() %>% 
  as.matrix()

#create a confusion matrix using absolute values
table(Predicted = pred, Actual = model.data$testtarget)

#predict future events targets
#pred_new <- model %>% 
#  predict_classes(model.data$future_matrix)
#
pred_new <- model %>% 
  predict(model.data$future_matrix) %>% 
  k_argmax() %>% 
  as.matrix()

#predict target probabilities
#prob_future<-model %>% 
#  predict_proba(model.data$future_matrix)

prob_future<-model %>% 
  predict(model.data$future_matrix)

future_rows<-as.numeric(nrow(prob_future))

prob_pred_future<-cbind(round(prob_future[1:future_rows,1:model.data$test_dim], 3),
                 pred_new[1:future_rows])

prob_pred_df <- as.data.frame(prob_pred_future)

#put down predictions for all matches to add to score_margin dataframe
data_mat<-rbind(model.data$data, model.data$full_future_matrix)

x<-data_mat[,2:col_num]

all_match_pred<-model %>% 
  predict(x)

score_data_lean<-cbind(score_data_lean, all_match_pred)
score_data_lean<- score_data_lean %>%  
  rename(pred_loss_prob = `1`, pred_win_prob = `2`)


score_data_lean<-score_data_lean %>%
  mutate(pred_cat = ifelse(pred_win_prob < 0.1, 1, 
                           ifelse(pred_win_prob > 0.1 & pred_win_prob < 0.2, 2,
                                  ifelse(pred_win_prob > 0.2 &pred_win_prob < 0.3, 3,
                                         ifelse(pred_win_prob > 0.3 & pred_win_prob < 0.4, 4,
                                                ifelse(pred_win_prob > 0.4 & pred_win_prob < 0.5, 5,
                                                       ifelse(pred_win_prob > 0.5 & pred_win_prob < 0.6, 6,
                                                              ifelse(pred_win_prob > 0.6 & pred_win_prob< 0.7, 7,
                                                                     ifelse(pred_win_prob > 0.7 & pred_win_prob < 0.8, 8,
                                                                            ifelse(pred_win_prob > 0.8 & pred_win_prob < 0.9, 9, 10))))))))))
#make numerical value categories
score_data_lean$pred_cat_factor <- as.factor(score_data_lean$pred_cat)

# New facet label names for supp variable
score_data_lean$pred_cat <- factor(score_data_lean$pred_cat_factor, levels = c(1,2,3,4,5,6,7,8,9,10), 
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))

# Find the mean of each group removing the unknown margins
Sum_pred_cat<-score_data_lean %>%
  group_by(pred_cat) %>% 
  filter(Margin != 999) %>% 
  summarise(rating.mean=mean(Margin), rating.sd = sd(Margin))

# linear formula for predicting margin from win probability
margin_formula <- score_data_lean %>% filter(Margin < 998)
formula <- lm(Margin ~ pred_win_prob, data= margin_formula)
score_data_lean %<>% mutate(margin_est_linear = predict(formula, newdata = score_data_lean)) # add estimate of margin
#score_data_lean %<>% mutate(margin_est_linear = -57+117*pred_win_prob-1.5) # add estimate of margin

