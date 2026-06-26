# tests/check_model_training_guard.R — model_training layer-config guard.
#
# The guard runs before any Keras call, so a length mismatch stops there and the
# failure paths are testable without Keras or training. Overriding the config
# globals works because model_training looks MODEL_UNITS/MODEL_DROPOUT up in the
# global env.
#
#   Rscript tests/check_model_training_guard.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# sanity: current config matches the 4-layer architecture (guard would pass)
if (length(MODEL_UNITS) != 4 || length(MODEL_DROPOUT) != 4)
  fail("config MODEL_UNITS/MODEL_DROPOUT no longer length 4 — architecture/test out of sync")

# too few units -> would feed NA to the 4th layer -> guard must stop
MODEL_UNITS <- c(256L, 128L, 64L)
e1 <- tryCatch({ model_training(NULL, NULL); "NO ERROR" }, error = function(e) conditionMessage(e))
if (!grepl("MODEL_UNITS", e1) || !grepl("match", e1))
  fail(paste("short MODEL_UNITS not guarded:", e1))

# restore units, over-long dropout -> guard must stop
MODEL_UNITS   <- c(256L, 128L, 64L, 32L)
MODEL_DROPOUT <- c(0.8, 0.5, 0.2, 0.1, 0.05)
e2 <- tryCatch({ model_training(NULL, NULL); "NO ERROR" }, error = function(e) conditionMessage(e))
if (!grepl("MODEL_DROPOUT", e2) || !grepl("match", e2))
  fail(paste("long MODEL_DROPOUT not guarded:", e2))

message("PASS  model_training guard: catches MODEL_UNITS/MODEL_DROPOUT length mismatch")
