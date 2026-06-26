# get_fixture.R — fixture adapter.
#
# Fetches the season fixture from a source and maps it to the project's canonical
# schema: columns (Date, Season, Season.Game, Round, Home.Team, Away.Team, Venue)
# and canonical team names. The source's quirks (column names, team-name spellings,
# UTC timestamps) are absorbed here, so swapping sources later means editing only
# this adapter — build_features() never sees them.
#
# Replaces the previous Squiggle fetch (fetch_fixture_squiggle), which is broken
# against current curl (curl_parse_url no longer exported).
#
# Team-name translation (afl_to_canonical_names) lives in functions/team_names.R.

# Returns the full season fixture in canonical form. round.no detection and
# per-round filtering stay in build_features() (they depend on Sys.Date()).
get_fixture <- function(season = as.numeric(format(Sys.Date(), "%Y")), source = "AFL") {
  raw <- fitzRoy::fetch_fixture(season = season, source = source)

  raw %>%
    dplyr::transmute(
      # UTC ISO8601 -> local AEST/AEDT, formatted to match the old Squiggle string
      # that downstream parses with as.Date(..., "%Y-%m-%d %H:%M:%S").
      Date        = format(lubridate::with_tz(lubridate::ymd_hms(utcStartTime),
                                              "Australia/Melbourne"),
                           "%Y-%m-%d %H:%M:%S"),
      Season      = compSeason.year,
      Season.Game = id,
      Round       = round.roundNumber,
      Home.Team   = afl_to_canonical_names(home.team.name),
      Away.Team   = afl_to_canonical_names(away.team.name),
      Venue       = venue.name
    )
}
