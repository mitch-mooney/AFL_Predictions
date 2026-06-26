# AFL Predictions

[![tests](https://github.com/mitch-mooney/AFL_Predictions/actions/workflows/tests.yml/badge.svg)](https://github.com/mitch-mooney/AFL_Predictions/actions/workflows/tests.yml)

A weekly AFL match prediction system using player statistics, Glicko ratings, and a Keras deep learning model.

## How It Works

1. **Data**: Player stats from Footywire, match results from AFLTables, fixtures from AFL.com — all via the `fitzRoy` package.
2. **Ratings**: Glicko2 ratings calculated per team using the `PlayerRatings` package.
3. **Features**: ~20 lagged and differential metrics (margins, scoring chains, disposals, ratings, season-to-date totals, etc.)
4. **Models**: Two dense neural networks (256→128→64→32→2). The odds-aware **primary** model (`model/model`) is used for matches that have current betting odds; a **betless** model (`model/model_betless`), trained without odds, is the fallback for the rest. ~71% test accuracy.
5. **Output**: Win probabilities and margin estimates for each upcoming match.

## Architecture

Each pipeline stage is a pure **core** (all the data logic, no IO) behind a thin **shell** (fetches, reads, writes). `run_pipeline.R` composes the shells as plain function calls — each takes explicit inputs and returns explicit outputs, with no cross-stage global state. The upcoming round is **auto-detected** from the fixture, so there's nothing to set by hand.

| Stage | File | Core | Shell |
|---|---|---|---|
| 1. Current-round odds | `source_code/betting_odds.R` | `build_round_odds()` | `fetch_round_odds()` |
| 2. Stats + features + ratings | `source_code/AFL_data.R` | `build_features_core()` | `build_features()` |
| 3. Predictions | `source_code/prediction_model.R` | `predict_round_core()` | `predict_round()` |
| 4. Archive + render | `source_code/Store_predictions.R` | `assemble_round_outputs()` | `store_round()` |
| (post-results) Archive odds | `source_code/Store_betting_odds.R` | `archive_betting_odds()` | `store_betting_odds()` |

The reusable feature logic lives in small, unit-tested helpers under `functions/` (home/away reshape, Glicko, lag/season/within-match features, team-name translation, odds parsing, prediction binning, the primary/betless override).

## Setup

### Requirements

Install R packages:
```r
install.packages(c(
  "tidyverse", "fitzRoy", "data.table", "PlayerRatings",
  "keras", "tensorflow", "plotly", "lubridate", "reshape2", "ggpmisc",
  "magrittr", "jsonlite", "ggpubr", "reactable", "htmltools",
  "sparkline", "knitr"
))
keras::install_keras()
```

### API Key

Betting odds require an API key from [the-odds-api.com](https://the-odds-api.com).
Add it to `~/.Renviron`:
```
ODDS_API_KEY=your_key_here
```
Then restart R or run `readRenviron("~/.Renviron")`. Without it, predictions still run using the betless model.

## Running the Pipeline

```r
source("run_pipeline.R")
```

The round is auto-detected from the fixture; there's nothing to configure. This runs the four stages in order:

1. `betting_odds.R` — fetch current-round odds from the-odds-api.com
2. `AFL_data.R` — fetch stats + fixture, build features, compute Glicko ratings
3. `prediction_model.R` — generate win probabilities and margin estimates
4. `Store_predictions.R` — archive predictions to CSV and render the output table

After the round is complete, archive the results + odds:
```r
source("source_code/Store_betting_odds.R")
store_betting_odds(round.no)
```

## Retraining

Retrain the Keras model on current data (e.g. after adding seasons or changing features — edit hyperparameters in `functions/config.R` first):
```r
source("source_code/AFL_data.R")
feat <- build_features(); future_data_lean <- feat$future_data_lean
source("source_code/retrain_model.R")
retrain_model()   # trains, archives a timestamped copy + training log, asks before overwriting the active model
```

## Automation (GitHub Actions)

- **`tests`** (`.github/workflows/tests.yml`) — runs the hermetic unit tests on every push / PR.
- **`run-pipeline`** (`.github/workflows/run-pipeline.yml`) — runs the full pipeline weekly (Thu 06:00 AEST) and on manual dispatch, then commits the refreshed predictions back to `main`. Needs an `ODD_API_KEY` repository secret (mapped to the `ODDS_API_KEY` env var the code reads).

## Testing

Behaviour is locked down two ways:

- **Hermetic unit tests** (`tests/check_*.R`) — synthetic data, no Keras/network/fixtures. These run in CI.
- **Golden snapshot tests** (`tests/capture_*` + `tests/verify_*`) — prove each stage's refactor is byte-identical to a captured baseline. Fixtures live in `tests/golden/` (gitignored, regenerable) and need Keras / network, so they run locally.

```r
Rscript tests/check_expand_home_away.R   # etc. — each prints PASS / exits non-zero on failure
```

## Project Structure

```
run_pipeline.R              # Entry point — run this each round
source_code/                # Pipeline stages (each = pure core + IO shell)
  betting_odds.R            #   Stage 1: current-round odds
  AFL_data.R                #   Stage 2: data, feature engineering, Glicko ratings
  prediction_model.R        #   Stage 3: load models, generate predictions
  Store_predictions.R       #   Stage 4: archive predictions + render table
  Store_betting_odds.R      #   Post-results: archive completed round odds + results
  retrain_model.R           #   Retrain the Keras model (retrain_model())
functions/                  # Shared, unit-tested helpers (auto-sourced at startup)
  config.R                  #   Project-wide constants
  team_names.R              #   Team-name registry: to_canonical/from_canonical + wrappers
  get_fixture.R             #   Fixture adapter (fitzRoy AFL.com source -> canonical)
  expand_home_away.R        #   Reshape one-row-per-match -> one-row-per-team
  glicko_ratings.R          #   Glicko2 ratings + history reshape
  add_lag_features.R        #   Lagged ("last_*") features in one grouped pass
  add_season_to_date.R      #   Season-to-date totals before each match
  match_aggregations.R      #   Within-match differentials / opponent values
  apply_primary_override.R  #   Override betless predictions with the odds-aware model
  bin_prediction.R          #   Bucket win probability into decile bins
  parse_odds_json.R         #   Parse the-odds-api JSON (line + h2h)
  wrangle_fixture.R         #   Reshape the upcoming fixture into home/away rows
  model_data_function.R     #   Normalise + split data for Keras
  model_training_function.R #   Keras model architecture + training
  simulation_function.R     #   Monte Carlo margin simulation
  reactable_function.R      #   Interactive ratings table
  custom_theme.R            #   ggplot2 theme
  extra_features.R          #   Candidate features for experimentation
csv_files/                  # Data storage
  AFLstats.csv              #   Historical player-level stats (2010-present)
  betting_odds.csv          #   Historical match results with betting odds
  betting_odds_round.csv    #   Current round's odds (one row per team)
  round_predictions.csv     #   All historical predictions
  Team_Ratings.csv          #   Latest Glicko ratings snapshot
model/
  model/                    #   Primary model (odds-aware) — used when current odds exist
  model_betless/            #   Betless model — fallback for matches without odds
  archive/                  #   Timestamped retrains (retrain_model())
tests/                      # Unit tests + golden snapshot verifications
analysis_code/              # Exploratory scripts (not part of the weekly pipeline)
images/                     # Team logo PNGs
.github/workflows/          # CI (tests) + scheduled pipeline run
```

## Key Constants (`functions/config.R`)

| Constant | Value | Description |
|---|---|---|
| `START_SEASON` | 2010 | First season included in training data |
| `TRAIN_TEST_SEED` | 321 | Random seed for train/test split |
| `MODEL_PATH` | `"model/model_betless"` | Betless model — fallback when odds are unavailable |
| `MODEL_PATH_FULL` | `"model/model"` | Primary odds-aware model |
| `MODEL_BETLESS` | `FALSE` | `FALSE` = primary model for matches with odds, betless for the rest |
| `MODEL_UNITS` / `MODEL_DROPOUT` | 4 layers | Hidden-layer sizes and dropout rates |
| `PRED_BINS` / `PRED_BIN_LABELS` | 10 deciles | Win-probability calibration bins |
| `AFL_TEAMS` | 18 teams | Canonical team-name list for numeric encoding |
