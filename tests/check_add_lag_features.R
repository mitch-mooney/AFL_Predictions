# tests/check_add_lag_features.R — unit test for the lag-feature helper.
# Proves it matches hand-written grouped lag(), including a lag-of-lag dependency.
#
#   Rscript tests/check_add_lag_features.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(dplyr))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

df <- tibble(
  Team = c("A", "A", "A", "B", "B"),
  Date = as.Date(c("2020-03-01", "2020-03-08", "2020-03-15", "2020-03-01", "2020-03-08")),
  x    = c(1, 2, 3, 4, 5),
  y    = c(10, 20, 30, 40, 50)
)

# (1) single lag matches manual grouped lag, order-independent of row order
shuffled <- df[c(3, 1, 5, 2, 4), ]
out    <- add_lag_features(shuffled, "Team", c(last_x = "x"))
manual <- shuffled %>% group_by(Team) %>% mutate(last_x = lag(x, order_by = Date)) %>% ungroup()
if (!identical(out$last_x, manual$last_x)) fail("single lag != manual grouped lag")

# (2) multiple lags + a lag-of-lag dependency (l2 lags l1 created in the same call)
out2 <- add_lag_features(df, "Team", c(l1 = "x", l2 = "l1", ly = "y"))
man2 <- df %>% group_by(Team) %>%
  mutate(l1 = lag(x, order_by = Date), l2 = lag(l1, order_by = Date), ly = lag(y, order_by = Date)) %>%
  ungroup()
if (!identical(out2$l1, man2$l1)) fail("l1 mismatch")
if (!identical(out2$l2, man2$l2)) fail("dependency l2 = lag(l1) mismatch")
if (!identical(out2$ly, man2$ly)) fail("ly mismatch")

# (3) multi-column group key + custom order column
df$Opp <- c("B", "B", "C", "A", "A")
df$ord <- as.integer(format(df$Date, "%Y%m%d"))
out3 <- add_lag_features(df, c("Team", "Opp"), c(le = "x"), order = "ord")
man3 <- df %>% group_by(Team, Opp) %>% mutate(le = lag(x, order_by = ord)) %>% ungroup()
if (!identical(out3$le, man3$le)) fail("multi-key/custom-order mismatch")

message("PASS  add_lag_features: matches grouped lag(), incl. lag-of-lag dependency")
