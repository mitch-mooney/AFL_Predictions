model_training <- function(inputs, target){
  ### --- Winning Model --- ### 71.3% accuracy on test data
  # Hyperparameters are set in functions/config.R (MODEL_UNITS, MODEL_DROPOUT, etc.)
  # To experiment: edit config.R, then retrain via source("source_code/retrain_model.R")

  # Activation sequence is specific to this architecture — do not generalise to all-relu
  activations <- c("relu", "sigmoid", "sigmoid", "relu")

  model <- keras_model_sequential()
  model %>%
    layer_dense(units = MODEL_UNITS[1], activation = activations[1],
                input_shape = c(col_num - 1)) %>%
    layer_dropout(rate = MODEL_DROPOUT[1]) %>%
    layer_dense(units = MODEL_UNITS[2], activation = activations[2]) %>%
    layer_dropout(rate = MODEL_DROPOUT[2]) %>%
    layer_dense(units = MODEL_UNITS[3], activation = activations[3]) %>%
    layer_dropout(rate = MODEL_DROPOUT[3]) %>%
    layer_dense(units = MODEL_UNITS[4], activation = activations[4]) %>%
    layer_dropout(rate = MODEL_DROPOUT[4]) %>%
    layer_dense(units = 2, activation = "softmax")

  summary(model)

  model %>%
    compile(
      loss      = "binary_crossentropy",
      optimizer = optimizer_adam(learning_rate = MODEL_LR),
      metrics   = "accuracy"
    )

  # Early stopping: stop when val_accuracy stops improving, restore best weights
  es <- callback_early_stopping(
    monitor              = "val_accuracy",
    patience             = MODEL_PATIENCE,
    restore_best_weights = TRUE,
    verbose              = 1
  )

  history <- model %>%
    fit(
      inputs,
      target,
      epochs           = MODEL_EPOCHS,
      batch_size       = MODEL_BATCH,
      validation_split = 0.3,
      callbacks        = list(es)
    )

  return(model)
}
