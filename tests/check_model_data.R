# tests/check_model_data.R — future-row detection in model_data().
#
# Engineers the case the old `firstRow:lastRow` range logic got wrong: future
# (target == 999) rows that are NON-CONTIGUOUS and not at the end. The old code
# would treat the whole span between them as future; the explicit which(col1==999)
# detection picks exactly the sentinel rows. Needs Keras (normalize/to_categorical).
#
#   Rscript tests/check_model_data.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(keras))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# 12 rows: target in col 1; future sentinel 999 at rows 5 and 10 (non-contiguous,
# with historical rows after the last sentinel). Old range logic -> rows 5:10.
data <- data.frame(
  target = c(0, 1, 0, 1, 999, 0, 1, 0, 1, 999, 0, 1),
  f1     = c(10, 22, 14, 36, 50, 61, 17, 38, 29, 70, 11, 42),
  f2     = c(1, 5, 2, 6, 3, 7, 2, 8, 4, 9, 1, 5)
)

md <- model_data(data)

if (nrow(md$future_matrix) != 2)
  fail(paste("expected 2 future rows (the two 999s), got", nrow(md$future_matrix)))
if (!all(md$full_future_matrix[, 1] == 999))
  fail("future rows are not the target==999 rows")
if (nrow(md$data) != 10)
  fail(paste("expected 10 non-future rows, got", nrow(md$data)))
if (any(md$data[, 1] == 999))
  fail("a 999 sentinel leaked into the training/test data")

message("PASS  model_data: detects non-contiguous future rows by target==999, no leakage")
