# tests/check_team_names.R — unit test for the unified team-name registry.
# Hermetic, no Keras/network. Guards the wrappers, including to_afltables_names
# (used in Store_betting_odds.R, which has no golden test).
#
#   Rscript tests/check_team_names.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }
eq   <- function(a, b, m) if (!identical(a, b)) fail(paste0(m, ": ", paste(a, collapse=",")))

# normalize: AFLTables + legacy Squiggle spellings -> canonical; canonical passes through
eq(normalize_team_names(c("Footscray", "Brisbane Lions", "Greater Western Sydney", "Adelaide", "GWS")),
   c("Western Bulldogs", "Brisbane", "GWS", "Adelaide", "GWS"),
   "normalize_team_names")

# odds first-word -> canonical
eq(expand_odds_team_names(c("Port", "St", "Western", "Greater", "Carlton")),
   c("Port Adelaide", "St Kilda", "Western Bulldogs", "GWS", "Carlton"),
   "expand_odds_team_names")

# AFL.com -> canonical
eq(afl_to_canonical_names(c("Adelaide Crows", "Gold Coast SUNS", "GWS GIANTS", "West Coast Eagles", "Carlton")),
   c("Adelaide", "Gold Coast", "GWS", "West Coast", "Carlton"),
   "afl_to_canonical_names")

# canonical -> AFLTables (GWS stays GWS — AFLTables already uses it; that asymmetry is intentional)
eq(to_afltables_names(c("Western Bulldogs", "Brisbane", "GWS", "Adelaide")),
   c("Footscray", "Brisbane Lions", "GWS", "Adelaide"),
   "to_afltables_names")

# round-trip: canonical -> afltables -> canonical is identity for every team
canon <- AFL_TEAMS
eq(normalize_team_names(to_afltables_names(canon)), canon, "afltables round-trip")

message("PASS  team-name registry: all 4 wrappers + afltables round-trip correct")
