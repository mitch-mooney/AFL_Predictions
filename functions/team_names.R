# team_names.R — one registry for translating team names across data sources.
#
# Every source-specific spelling that differs from the project's canonical name
# lives in TEAM_NAME_ALIASES (source, alias, canonical). to_canonical() and
# from_canonical() are the generic engine; the named wrappers below are thin
# shims kept so existing call sites are unchanged.
#
# Sources:
#   afltables — fetch_results_afltables(): Footscray, Brisbane Lions (GWS already canonical)
#   squiggle  — legacy fixture source (dead): Greater Western Sydney. Kept so
#               normalize_team_names() stays byte-identical; harmless no-op now.
#   odds      — the-odds-api first word (after strsplit): Port, St, Gold, ...
#   afl       — fetch_fixture(source = 'AFL'): Adelaide Crows, Gold Coast SUNS, ...
#
# Base R only (no tibble) — this file is sometimes sourced before tidyverse loads.
TEAM_NAME_ALIASES <- data.frame(
  source = c("afltables", "afltables", "squiggle",
             "odds", "odds", "odds", "odds", "odds", "odds", "odds",
             "afl", "afl", "afl", "afl", "afl", "afl", "afl"),
  alias = c("Footscray", "Brisbane Lions", "Greater Western Sydney",
            "Port", "St", "Gold", "Western", "North", "West", "Greater",
            "Adelaide Crows", "Brisbane Lions", "Geelong Cats", "Gold Coast SUNS",
            "GWS GIANTS", "Sydney Swans", "West Coast Eagles"),
  canonical = c("Western Bulldogs", "Brisbane", "GWS",
                "Port Adelaide", "St Kilda", "Gold Coast", "Western Bulldogs",
                "North Melbourne", "West Coast", "GWS",
                "Adelaide", "Brisbane", "Geelong", "Gold Coast",
                "GWS", "Sydney", "West Coast"),
  stringsAsFactors = FALSE
)

# Translate source-specific names TO canonical. `from` is one or more source keys.
# Names not found in those sources pass through unchanged.
to_canonical <- function(x, from) {
  m <- TEAM_NAME_ALIASES[TEAM_NAME_ALIASES$source %in% from, ]
  lookup <- stats::setNames(m$canonical, m$alias)
  ifelse(x %in% names(lookup), unname(lookup[x]), x)
}

# Translate canonical names TO a single source's spelling. Unmapped names pass through.
from_canonical <- function(x, to) {
  m <- TEAM_NAME_ALIASES[TEAM_NAME_ALIASES$source %in% to, ]
  lookup <- stats::setNames(m$alias, m$canonical)
  ifelse(x %in% names(lookup), unname(lookup[x]), x)
}

# --- thin wrappers (unchanged interfaces) ------------------------------------

# AFLTables / Squiggle source names -> canonical
normalize_team_names <- function(x) to_canonical(x, from = c("afltables", "squiggle"))

# the-odds-api first-word names -> canonical (apply after strsplit first word)
expand_odds_team_names <- function(x) to_canonical(x, from = "odds")

# fitzRoy AFL.com source names -> canonical
afl_to_canonical_names <- function(x) to_canonical(x, from = "afl")

# canonical -> AFLTables names, for joining with fetch_results_afltables()
to_afltables_names <- function(x) from_canonical(x, to = "afltables")
