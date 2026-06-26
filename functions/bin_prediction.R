# bin_prediction.R — bucket a win probability into one of 10 decile bins (1..10).
#
# Replaces the nested-ifelse binning that was duplicated in prediction_model.R and
# analysis_code/evaluation_script.R. Breakpoints live in config.R (PRED_BINS);
# PRED_BIN_LABELS gives the matching "0-10%" .. "90-100%" labels.
#
# Boundary behaviour: an exact tenth bins correctly (0.1 -> 2, 0.5 -> 6), unlike
# the old nested ifelse which let exact tenths fall through to bin 10.
bin_prediction <- function(p) {
  findInterval(p, PRED_BINS) + 1L
}
