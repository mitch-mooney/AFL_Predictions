# fetch_with_retry() — re-evaluate a fetch expression when the upstream source
# fails transiently.
#
# The weekly pipeline pulls from third-party sites (AFLTables, Footywire) that
# occasionally refuse a connection from CI. A single blip used to end the run,
# so the expression is retried before the error is allowed through.
#
#   results <- fetch_with_retry(fetch_results_afltables(season = 2010:2026))

fetch_with_retry <- function(expr, attempts = 3, wait = 120, sleep_fn = Sys.sleep) {
  # keep `expr` unevaluated so each attempt re-runs the fetch. substitute()/eval()
  # is R's idiom for that; it re-runs the caller's own literal call in the caller's
  # frame, and never parses a string, so there is no injection surface here.
  fetch <- substitute(expr)
  env   <- parent.frame()

  for (i in seq_len(attempts)) {
    out <- tryCatch(eval(fetch, env), error = function(e) e)
    if (!inherits(out, "error")) return(out)

    # out of attempts — let the caller's own handler decide what a dead source means
    if (i == attempts) stop(out)

    # announce it: a silent retry looks like a hung job in the CI log
    message("Fetch failed (attempt ", i, "/", attempts, "): ",
            deparse(fetch)[1], " — ", conditionMessage(out),
            "\nRetrying in ", wait, "s...")
    sleep_fn(wait)
  }
}
