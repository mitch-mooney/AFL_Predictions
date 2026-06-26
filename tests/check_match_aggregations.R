# tests/check_match_aggregations.R — unit test for the within-match helpers.
# Hermetic, no deps beyond dplyr.
#
#   Rscript tests/check_match_aggregations.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(dplyr))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }
eq   <- function(a, b, m) if (!isTRUE(all.equal(a, b))) fail(paste0(m, ": ", paste(a, collapse = ",")))

# a match's two team values
eq(match_diff(c(10, 4)),     c(6, -6), "match_diff")       # 10-4, 4-10
eq(opponent_value(c(10, 4)), c(4, 10), "opponent_value")   # opp of each

# relationship: match_diff(x) == x - opponent_value(x)
x <- c(10, 4)
eq(match_diff(x), x - opponent_value(x), "diff == x - opp")

# inside a grouped mutate, per match
df  <- tibble(Match_id = c(1, 1, 2, 2), x = c(10, 4, 7, 3))
out <- df %>% group_by(Match_id) %>%
  mutate(d = match_diff(x), o = opponent_value(x)) %>% ungroup()
eq(out$d, c(6, -6, 4, -4), "grouped match_diff")
eq(out$o, c(4, 10, 3, 7),  "grouped opponent_value")

# NA propagates (no na.rm) — matches the original inline expressions
if (!all(is.na(match_diff(c(5, NA)))))     fail("match_diff should propagate NA")
if (!all(is.na(opponent_value(c(5, NA))))) fail("opponent_value should propagate NA")

message("PASS  match_aggregations: match_diff / opponent_value correct, NA-propagating")
