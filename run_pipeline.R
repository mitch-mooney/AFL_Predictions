# run_pipeline.R — weekly prediction pipeline
#
# Run this script each round. round.no is auto-detected from the fixture.
# After results come in, run Store_betting_odds.R separately to archive them.
#
# Execution order:
#   1. betting_odds.R      — fetch current round odds from the-odds-api.com
#   2. AFL_data.R          — fetch stats + fixture (auto-detects round), build features, Glicko ratings
#   3. prediction_model.R  — generate win probabilities and margin estimates
#   4. Store_predictions.R — archive predictions to CSV and render output table
#
# After the round is complete:
#   5. Store_betting_odds.R — archive results + odds to betting_odds.csv

message("=== AFL Predictions Pipeline — ", as.numeric(format(Sys.Date(), "%Y")), " ===")

message("\n[1/4] Fetching betting odds (optional — predictions still run without them)...")
source("source_code/betting_odds.R")   # bootstrap + defines fetch_round_odds()
betting_join <- tryCatch(
  fetch_round_odds(),
  error = function(e) {
    warning("betting_odds fetch failed: ", conditionMessage(e),
            "\nProceeding without current round odds.")
    NULL
  }
)

message("\n[2/4] Fetching stats and building features...")
source("source_code/AFL_data.R")   # defines build_features_core() + build_features()
features <- build_features(betting_join = betting_join)
# Destructure into the global names the downstream stages still read.
future_data_lean <- features$future_data_lean
future_data_full <- features$future_data_full
score_data_lean  <- features$score_data_lean
round            <- features$round
glicko_rate      <- features$glicko_rate
round.no         <- features$round.no

message("\n[3/4] Generating predictions...")
source("source_code/prediction_model.R")

message("\n[4/4] Archiving predictions and rendering table...")
source("source_code/Store_predictions.R")

message("\nDone. Predictions saved to csv_files/round_predictions.csv")
message("Run source('source_code/Store_betting_odds.R') after results are in.")

message("\n=== ", current_round, " — ", current_season, " Selections ===")
print(htmltools::browsable(predictions_table))
