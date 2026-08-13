# tests/check_fetch_with_retry.R — unit test for the fetch retry helper.
# Proves a transient upstream failure is retried rather than killing the pipeline
# run, and that a genuinely-down source still raises after the attempts are spent.
# Hermetic: no network, and sleeping is injected so the test runs instantly.
#
#   Rscript tests/check_fetch_with_retry.R

setwd(rprojroot::find_root(rprojroot::has_file("run_pipeline.R")))
invisible(sapply(paste0("functions/", list.files("functions/")), source))

fail <- function(m) { message("FAIL  ", m); quit(status = 1) }

# a fetch that fails its first `n_fail` calls, then succeeds — stands in for a
# flaky upstream (afltables timing out, then recovering)
flaky <- function(n_fail, value = "data") {
  calls <- 0
  function() {
    calls <<- calls + 1
    if (calls <= n_fail) stop("cannot open the connection")
    value
  }
}

# --- 1. success first time: value returned, no retry, no sleep ---------------
slept <- c()
src   <- flaky(0, value = "results")
out   <- fetch_with_retry(src(), sleep_fn = function(s) slept <<- c(slept, s))
if (!identical(out, "results")) fail(paste("first-try value:", out))
if (length(slept) != 0)         fail(paste("slept on a successful first try:", paste(slept, collapse = ",")))

# --- 2. fails once, then succeeds: later attempt's value is returned ---------
slept <- c()
src   <- flaky(1, value = "results")
out   <- fetch_with_retry(src(), sleep_fn = function(s) slept <<- c(slept, s))
if (!identical(out, "results")) fail(paste("value after one transient failure:", out))
if (length(slept) != 1)         fail(paste("expected 1 wait, got", length(slept)))

# --- 3. never recovers: the error is re-raised once attempts are spent -------
# (this is what keeps a genuinely-down AFLTables a hard failure rather than a
#  silent NULL that would poison the ratings downstream)
slept <- c()
src   <- flaky(99)
out   <- tryCatch(fetch_with_retry(src(), sleep_fn = function(s) slept <<- c(slept, s)),
                  error = function(e) e)
if (!inherits(out, "error"))                              fail(paste("expected an error, got:", paste(out, collapse = ",")))
if (!grepl("cannot open the connection", conditionMessage(out))) fail(paste("lost the original error:", conditionMessage(out)))
if (length(slept) != 2)                                   fail(paste("expected 2 waits over 3 attempts, got", length(slept)))

# --- 4. waits the configured interval between attempts -----------------------
# default is 3 attempts two minutes apart: the CI outage that prompted this
# helper was a ~10s connect timeout, so the gap must be long enough to outlast one
slept <- c()
src   <- flaky(99)
invisible(tryCatch(fetch_with_retry(src(), sleep_fn = function(s) slept <<- c(slept, s)),
                   error = function(e) NULL))
if (!identical(slept, c(120, 120))) fail(paste("default backoff:", paste(slept, collapse = ",")))

slept <- c()
src   <- flaky(99)
invisible(tryCatch(fetch_with_retry(src(), attempts = 4, wait = 5,
                                    sleep_fn = function(s) slept <<- c(slept, s)),
                   error = function(e) NULL))
if (!identical(slept, c(5, 5, 5))) fail(paste("configured backoff:", paste(slept, collapse = ",")))

# --- 5. announces each retry, so a stalled CI run is diagnosable from the log -
src  <- flaky(1, value = "results")
msgs <- capture.output(
  invisible(fetch_with_retry(src(), sleep_fn = function(s) NULL)),
  type = "message")
joined <- paste(msgs, collapse = " ")
if (!grepl("attempt 1/3", joined))                    fail(paste("no attempt counter in log:", joined))
if (!grepl("cannot open the connection", joined))     fail(paste("no cause in log:", joined))
if (!grepl("src\\(\\)", joined))                      fail(paste("no source named in log:", joined))

message("PASS  fetch_with_retry: transient failures retried, exhausted attempts re-raise")
