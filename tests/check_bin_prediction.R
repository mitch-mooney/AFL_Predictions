# tests/check_bin_prediction.R — unit test for the decile binning helper.
# Hermetic, no Keras. Documents the boundary fix vs the old nested ifelse.
#
#   Rscript tests/check_bin_prediction.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# interior values -> the obvious decile (matches the old nested ifelse)
interior <- c(0.05, 0.15, 0.35, 0.55, 0.85, 0.95)
expected <- c(1, 2, 4, 6, 9, 10)
if (!identical(as.integer(bin_prediction(interior)), as.integer(expected)))
  fail(paste("interior bins wrong:", paste(bin_prediction(interior), collapse = ",")))

# extremes
if (bin_prediction(0) != 1)   fail("0 should bin to 1")
if (bin_prediction(1) != 10)  fail("1 should bin to 10")

# boundary fix: exact tenths bin correctly (old ifelse sent these to bin 10)
if (bin_prediction(0.1) != 2) fail(paste("0.1 should bin to 2, got", bin_prediction(0.1)))
if (bin_prediction(0.5) != 6) fail(paste("0.5 should bin to 6, got", bin_prediction(0.5)))
if (bin_prediction(0.9) != 10) fail(paste("0.9 should bin to 10, got", bin_prediction(0.9)))

# labels line up: 10 bins, 10 labels
if (length(PRED_BIN_LABELS) != 10) fail("expected 10 bin labels")

message("PASS  bin_prediction: interior + extremes + boundary tenths bin correctly")
