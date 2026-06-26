# tests/check_add_season_to_date.R — unit test for the season-to-date helper.
# Proves it matches manual grouped lag(cumsum()), resets per season, and that a
# lagged NA on an upcoming row doesn't corrupt earlier values.
#
#   Rscript tests/check_add_season_to_date.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
suppressMessages(library(dplyr))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# season-to-date resets each (Season, Team); value is the total BEFORE this match
df <- tibble(
  Season = c(2024, 2024, 2024, 2025, 2025),
  Team   = c("A",  "A",  "A",  "A",  "A"),
  points = c(80,   90,   70,   100,  110)
)
out <- add_season_to_date(df, c(season_for = "points"))
#   2024 A: lag(cumsum(80,90,70))=0,80,170 ; 2025 A: 0,100
if (!isTRUE(all.equal(out$season_for, c(0, 80, 170, 0, 100)))) fail(paste("season_for:", paste(out$season_for, collapse = ",")))

man <- df %>% group_by(Season, Team) %>%
  mutate(season_for = lag(cumsum(points), 1, default = 0)) %>% ungroup()
if (!identical(out$season_for, man$season_for)) fail("!= manual lag(cumsum())")

# upcoming row (NA source) gets the prior cumsum via the lag, not NA
df2 <- tibble(Season = c(2024, 2024, 2024), Team = "A", points = c(80, 90, NA))
out2 <- add_season_to_date(df2, c(sf = "points"))
if (!isTRUE(all.equal(out2$sf, c(0, 80, 170)))) fail(paste("NA upcoming row:", paste(out2$sf, collapse = ",")))

# two columns at once + the default group
out3 <- add_season_to_date(
  tibble(Season = 2024, Team = "A", points = c(80, 90), opp_points = c(70, 60)),
  c(season_for = "points", season_against = "opp_points"))
if (!isTRUE(all.equal(out3$season_for, c(0, 80))) || !isTRUE(all.equal(out3$season_against, c(0, 70))))
  fail("two-column case wrong")

message("PASS  add_season_to_date: lag(cumsum()) before-this-match, season reset, NA-safe")
