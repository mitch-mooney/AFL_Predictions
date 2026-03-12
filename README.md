# AFL Predictions

A weekly AFL match prediction system using player statistics, Glicko ratings, and a Keras deep learning model.

## How It Works

1. **Data**: Player stats from Footywire, match results from AFLTables, fixtures from Squiggle — all via the `fitzRoy` package.
2. **Ratings**: Glicko2 ratings calculated per team using the `PlayerRatings` package.
3. **Features**: ~20 lagged and differential metrics (margins, scoring chains, disposals, ratings, etc.)
4. **Model**: Dense neural network (256→128→64→32→2) trained on binary win/loss classification. ~71% test accuracy.
5. **Output**: Win probabilities and margin estimates for each upcoming match.

## Setup

### Requirements

Install R packages:
```r
install.packages(c(
  "tidyverse", "fitzRoy", "data.table", "PlayerRatings",
  "keras", "plotly", "lubridate", "reshape2", "ggpmisc",
  "magrittr", "jsonlite", "ggpubr", "reactable", "htmltools",
  "sparkline", "formattable", "here"
))
keras::install_keras()
```

### API Key

Betting odds require an API key from [the-odds-api.com](https://the-odds-api.com).
Add it to `~/.Renviron`:
```
ODDS_API_KEY=your_key_here
```
Then restart R or run `readRenviron("~/.Renviron")`.

## Running the Pipeline

Set `round.no` and `YEAR` in `run_pipeline.R`, then source it:

```r
source("run_pipeline.R")
```

This runs the four steps in order:
1. `source_code/betting_odds.R` — fetch current round odds
2. `source_code/AFL_data.R` — fetch stats, build features, compute Glicko ratings
3. `source_code/prediction_model.R` — generate win probabilities and margin estimates
4. `source_code/Store_predictions.R` — archive to CSV and render output table

After the round is complete, archive results:
```r
source("source_code/Store_betting_odds.R")
```

## Project Structure

```
run_pipeline.R          # Entry point — run this each round
source_code/            # Main pipeline scripts
  AFL_data.R            # Data fetching, feature engineering, Glicko ratings
  betting_odds.R        # Fetch current round odds from the-odds-api.com
  prediction_model.R    # Load model and generate predictions
  Store_predictions.R   # Archive predictions and render output table
  Store_betting_odds.R  # Archive completed round results (run after round)
functions/              # Shared utilities (auto-sourced at startup)
  config.R              # Project-wide constants (START_SEASON, AFL_TEAMS, etc.)
  team_names.R          # Team name normalisation utilities
  model_data_function.R # Normalise and split data for Keras
  model_training_function.R  # Keras model architecture and training
  line_odds_function.R  # Parse spreads JSON from odds API
  h2h_odds_function.R   # Parse h2h JSON from odds API
  wrangle_fixture.R     # Reshape fixture into home/away format
  simulation_function.R # Monte Carlo margin simulation
  reactable_function.R  # Interactive ratings table
  custom_theme.R        # ggplot2 theme
csv_files/              # Data storage
  AFLstats.csv          # Historical player-level stats (2010–present)
  betting_odds.csv      # Historical match results with betting odds
  round_predictions.csv # All historical predictions
  Team_Ratings.csv      # Latest Glicko ratings snapshot
model/
  model_betless/        # Trained Keras model (betting odds excluded from inputs)
analysis_code/          # Exploratory scripts (not part of the weekly pipeline)
images/                 # Team logo PNGs
```

## Key Constants (`functions/config.R`)

| Constant | Value | Description |
|---|---|---|
| `START_SEASON` | 2010 | First season included in training data |
| `TRAIN_TEST_SEED` | 321 | Random seed for train/test split |
| `MODEL_PATH` | `"model/model_betless"` | Path to the trained Keras model |
| `AFL_TEAMS` | 18 teams | Canonical team name list for numeric encoding |
