# config.R — project-wide constants, sourced automatically via functions/ loader

# Training data start year
START_SEASON <- 2010

# Reproducibility seed for train/test split
TRAIN_TEST_SEED <- 321

# Monte Carlo simulation runs
SIM_N_RUNS <- 10000

# Paths to trained Keras models
MODEL_PATH      <- "model/model_betless"  # betless model — always used as fallback
MODEL_PATH_FULL <- "model/model"          # primary model — used when current-round odds are available

# When TRUE: betless model used for all matches regardless of odds availability.
# When FALSE: primary model used for matches with odds, betless as fallback for the rest.
MODEL_BETLESS <- FALSE

# Model architecture & training hyperparameters
# Edit these to experiment with different architectures, then retrain via retrain_model.R
MODEL_UNITS    <- c(256L, 128L, 64L, 32L)   # units per hidden dense layer
MODEL_DROPOUT  <- c(0.8, 0.5, 0.2, 0.1)    # dropout rate after each hidden layer
MODEL_EPOCHS   <- 600L                       # max epochs (early stopping will usually cut this short)
MODEL_BATCH    <- 256L                       # batch size
MODEL_LR       <- 0.002                      # Adam learning rate
MODEL_PATIENCE <- 30L                        # early stopping patience on val_accuracy

# Canonical AFL team names (alphabetical) — used for ordered() encoding
AFL_TEAMS <- c(
  "Adelaide", "Brisbane", "Carlton", "Collingwood", "Essendon", "Fremantle",
  "Geelong", "Gold Coast", "GWS", "Hawthorn", "Melbourne", "North Melbourne",
  "Port Adelaide", "Richmond", "St Kilda", "Sydney", "West Coast", "Western Bulldogs"
)
