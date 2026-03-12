library(keras)

# Betless model always runs first — provides predictions for all matches as the baseline/fallback
data <- future_data_lean %>% select(-matchType)
col_num<-as.numeric(ncol(data))
data[1:col_num] <- lapply(data[1:col_num], as.numeric) #make sure all variables are numeric

# returns a list of matrix used for running model
model.data <- model_data(data)
# load pre-trained model (to retrain, use source("source_code/retrain_model.R"))
#model <- model_training(inputs = model.data$full_data_matrix, target = model.data$full_data_target)
#model %>% save_model_tf("model/model_betless")
model <- tryCatch(
  load_model_tf(MODEL_PATH),
  error = function(e) stop("Failed to load Keras model from '", MODEL_PATH, "': ", conditionMessage(e))
)
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

future_data_lean<-cbind(future_data_lean, all_match_pred)
future_data_lean<- future_data_lean %>%
  rename(pred_loss_prob = `1`, pred_win_prob = `2`)

# --- Primary model override ---
# When MODEL_BETLESS = FALSE, replace betless predictions for matches where current odds exist.
# Matches without odds retain their betless predictions as a fallback.
if (!MODEL_BETLESS && exists("future_data_full")) {
  future_rows_with_odds <- future_data_full %>%
    filter(results == 999) %>%
    select(team, opposition, Season, status)

  if (nrow(future_rows_with_odds) > 0) {
    col_num_full <- ncol(future_data_full)
    data_full <- future_data_full
    data_full[1:col_num_full] <- lapply(data_full[1:col_num_full], as.numeric)
    model.data.full <- model_data(data_full)

    model_primary <- tryCatch(
      load_model_tf(MODEL_PATH_FULL),
      error = function(e) {
        warning("Primary model '", MODEL_PATH_FULL, "' failed to load — using betless for all matches.")
        NULL
      }
    )

    if (!is.null(model_primary)) {
      prob_primary <- model_primary %>% predict(model.data.full$future_matrix)

      primary_preds <- future_rows_with_odds %>%
        mutate(pred_loss_prob_primary = prob_primary[, 1],
               pred_win_prob_primary  = prob_primary[, 2])

      future_data_lean <- future_data_lean %>%
        left_join(primary_preds, by = c("team", "opposition", "Season", "status")) %>%
        mutate(
          pred_loss_prob = ifelse(!is.na(pred_loss_prob_primary) & results == 999,
                                  pred_loss_prob_primary, pred_loss_prob),
          pred_win_prob  = ifelse(!is.na(pred_win_prob_primary) & results == 999,
                                  pred_win_prob_primary, pred_win_prob)
        ) %>%
        select(-pred_loss_prob_primary, -pred_win_prob_primary)

      n_primary  <- nrow(future_rows_with_odds)
      n_fallback <- sum(future_data_lean$results == 999) - n_primary
      message(n_primary, " match(es) predicted with primary model (", MODEL_PATH_FULL, "), ",
              n_fallback, " with betless fallback (", MODEL_PATH, ").")
    }
  }
}


future_data_lean<-future_data_lean %>%
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
future_data_lean$pred_cat_factor <- as.factor(future_data_lean$pred_cat)

# New facet label names for supp variable
future_data_lean$pred_cat <- factor(future_data_lean$pred_cat_factor, levels = c(1,2,3,4,5,6,7,8,9,10), 
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))

# Find the mean of each group removing the unknown margins
Sum_pred_cat<-future_data_lean %>%
  left_join(
    score_data_lean %>% 
      select(Margin, Season, team, opposition, status, venue, matchType)
  ) %>%
  group_by(pred_cat) %>% 
  filter(Margin != 999) %>% 
  summarise(rating.mean=mean(Margin), rating.sd = sd(Margin))

# linear formula for predicting margin from win probability
margin_formula <- future_data_lean %>% 
  left_join(
    score_data_lean %>% 
      select(Margin, Season, team, opposition, status, venue, matchType)
  ) %>% 
  filter(Margin < 998)
formula <- lm(Margin ~ pred_win_prob, data= margin_formula)
future_data_lean %<>% mutate(margin_est_linear = predict(formula, newdata = future_data_lean)) # add estimate of margin
#score_data_lean %<>% mutate(margin_est_linear = -57+117*pred_win_prob-1.5) # add estimate of margin

