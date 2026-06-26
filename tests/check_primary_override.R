# tests/check_primary_override.R — hermetic unit test for apply_primary_override.
# No Keras: exercises the override rules on hand-built probability rows.
#
#   Rscript tests/check_primary_override.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(dplyr))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

# Betless-predicted rows (pred_*_prob already attached).
fdl <- tibble::tibble(
  team           = c(1, 3, 1, 5),
  opposition     = c(2, 4, 2, 6),
  Season         = c(2026, 2026, 2025, 2026),
  status         = c(1, 1, 1, 1),
  results        = c(999, 999, 1, 0),       # r1,r2 upcoming; r3,r4 historical
  pred_loss_prob = c(0.40, 0.45, 0.30, 0.50),
  pred_win_prob  = c(0.60, 0.55, 0.70, 0.50)
)

# Primary predictions: match r1 (upcoming) and r4 (historical, must be ignored).
primary_preds <- tibble::tibble(
  team = c(1, 5), opposition = c(2, 6), Season = c(2026, 2026), status = c(1, 1),
  pred_loss_prob_primary = c(0.20, 0.10),
  pred_win_prob_primary  = c(0.80, 0.90)
)

out <- apply_primary_override(fdl, primary_preds)
fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

if (nrow(out) != 4) fail(paste("row count changed:", nrow(out)))
if (any(grepl("_primary$", names(out)))) fail("primary helper columns not dropped")

if (!isTRUE(all.equal(out$pred_win_prob[1], 0.80)))  fail("r1 (999, matched) not overridden")
if (!isTRUE(all.equal(out$pred_loss_prob[1], 0.20))) fail("r1 loss not overridden")
if (!isTRUE(all.equal(out$pred_win_prob[2], 0.55)))  fail("r2 (999, unmatched) should keep betless")
if (!isTRUE(all.equal(out$pred_win_prob[3], 0.70)))  fail("r3 (historical) should keep betless")
if (!isTRUE(all.equal(out$pred_win_prob[4], 0.50)))  fail("r4 (matched but results != 999) must keep betless")

message("PASS  apply_primary_override: overrides only matched results==999 rows; rest keep betless")
