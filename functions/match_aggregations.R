# match_aggregations.R — within-match (2-row Match_id group) helpers.
#
# Use inside group_by(Match_id) %>% mutate(): a match has two rows (the two
# teams), so sum(x) is the pair total, and:
#   match_diff(x)     = this team's x minus the opponent's   (2*x - sum(x))
#   opponent_value(x) = the opponent's x                     (sum(x) - x)
# By construction, match_diff(x) == x - opponent_value(x).
#
# No na.rm: if either team's value is NA the pair sum is NA, matching the
# original inline expressions.
match_diff <- function(x) 2 * x - sum(x)

opponent_value <- function(x) sum(x) - x
