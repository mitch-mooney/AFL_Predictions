# tests/check_retrain_model.R — smoke test for the retrain path.
#
# Runs retrain_model() end-to-end on tiny synthetic data with 1 epoch and ALL
# side effects redirected to a temp dir (archive_dir / log_path / active_model_path)
# and overwrite = FALSE, so the real model/model_betless is never touched. Asserts
# the path completes and produces the expected artifacts. Needs Keras.
#
#   Rscript tests/check_retrain_model.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(keras))
invisible(sapply(paste0("functions/", list.files("functions/")), source))
source("source_code/retrain_model.R")

MODEL_EPOCHS <- 1L   # keep the smoke test fast (overrides config's 600)

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# future_data_lean-shaped: col 1 = target (0/1 historical + 999 future), 10 features
n_hist <- 40L; n_fut <- 4L; n_feat <- 10L
n <- n_hist + n_fut
target <- c(rep(c(0, 1), n_hist / 2), rep(999, n_fut))
feats  <- sapply(1:n_feat, function(j) ((seq_len(n) * j) %% 97) + j)   # varied, non-constant
data   <- data.frame(target = target, feats)

tmp <- file.path(tempdir(), "retrain_smoke")
unlink(tmp, recursive = TRUE); dir.create(tmp, recursive = TRUE)
archive_dir <- file.path(tmp, "archive")
log_path    <- file.path(tmp, "training_log.csv")
active_path <- file.path(tmp, "active_model")

m <- retrain_model(
  data              = data,
  active_model_path = active_path,
  model_label       = "smoke",
  archive_dir       = archive_dir,
  log_path          = log_path,
  overwrite         = FALSE
)

if (is.null(m))                                fail("retrain_model returned NULL")
if (length(list.files(archive_dir)) == 0)      fail("no archived model written")
if (!file.exists(log_path))                    fail("training log not written")
lg <- read.csv(log_path)
if (nrow(lg) != 1 || lg$n_features != n_feat)  fail(paste("training log wrong:", nrow(lg), "rows"))
if (dir.exists(active_path) || file.exists(active_path))
  fail("active model written despite overwrite = FALSE")

unlink(tmp, recursive = TRUE)
message("PASS  retrain_model smoke: trains, archives + logs to temp, leaves active model untouched")
