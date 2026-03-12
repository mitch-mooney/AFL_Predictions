# team_names.R — utilities for normalising AFL team names across data sources

# Converts various source formats to the canonical names used in this project
# Handles: AFLTables (Footscray, Brisbane Lions) and Squiggle (Greater Western Sydney)
normalize_team_names <- function(x) {
  x <- stringr::str_replace_all(x, stringr::fixed("Footscray"),              "Western Bulldogs")
  x <- stringr::str_replace_all(x, stringr::fixed("Brisbane Lions"),         "Brisbane")
  x <- stringr::str_replace_all(x, stringr::fixed("Greater Western Sydney"), "GWS")
  x
}

# Converts first-word-only names from the odds API to canonical names.
# Applied after sapply(strsplit(x, " "), `[`, 1) extracts the first word.
expand_odds_team_names <- function(x) {
  lookup <- c(
    "Port"    = "Port Adelaide",
    "St"      = "St Kilda",
    "Gold"    = "Gold Coast",
    "Western" = "Western Bulldogs",
    "North"   = "North Melbourne",
    "West"    = "West Coast",
    "Greater" = "GWS"
  )
  ifelse(x %in% names(lookup), lookup[x], x)
}

# Converts canonical names back to AFLTables format for joining with fetch_results_afltables()
to_afltables_names <- function(x) {
  x <- stringr::str_replace_all(x, stringr::fixed("Western Bulldogs"), "Footscray")
  x <- stringr::str_replace_all(x, stringr::fixed("Brisbane"),         "Brisbane Lions")
  x
}
