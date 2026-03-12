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
tryCatch(
  source("source_code/betting_odds.R"),
  error = function(e) warning("betting_odds.R failed: ", conditionMessage(e),
                               "\nProceeding without current round odds.")
)

message("\n[2/4] Fetching stats and building features...")
source("source_code/AFL_data.R")

message("\n[3/4] Generating predictions...")
source("source_code/prediction_model.R")

message("\n[4/4] Archiving predictions and rendering table...")
source("source_code/Store_predictions.R")

message("\nDone. Predictions saved to csv_files/round_predictions.csv")
message("Run source('source_code/Store_betting_odds.R') after results are in.")

message("\n=== ", current_round, " — ", current_season, " Selections ===")
print(htmltools::browsable(predictions_table))
