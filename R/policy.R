# Shared plumbing for the workflow features: a dependency guard that avoids an
# undeclared `rlang`, a stable result-combiner, and one fail policy so the CLI,
# Action, knit gate, and report do not each reinvent thresholds.

#' Require an optional package without depending on `rlang`.
#' @noRd
need_pkg <- function(pkg, why = "for this feature") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required {why}.",
      "i" = "Install it with {.code install.packages(\"{pkg}\")}."
    ))
  }
  invisible(TRUE)
}

#' Combine several `retraction_result` objects, preserving column types.
#'
#' Row-binds via plain data frames (identical schema preserves the `Date`
#' columns) and restores the class, rather than `rbind()` on tibbles which drops
#' both class and, on differing columns, alignment.
#' @noRd
bind_results <- function(results) {
  results <- Filter(function(r) inherits(r, "retraction_result"), results)
  if (!length(results)) cli::cli_abort("No results to combine.")
  combined <- do.call(rbind, lapply(results, as.data.frame))
  tbl <- tibble::as_tibble(combined)
  class(tbl) <- c("retraction_result", class(tbl))
  tbl
}

#' A fail policy shared by the CLI, GitHub Action, and knit gate.
#'
#' @param on Which result states should cause failure. Any of `"flagged"`,
#'   `"possible"`, `"unchecked"`, `"error"` (a file that failed to parse or
#'   fetch). Default fails only on confirmed flags.
#' @return A `retraction_fail_policy` object.
#' @export
fail_policy <- function(on = "flagged") {
  on <- match.arg(on, c("flagged", "possible", "unchecked", "error"),
                  several.ok = TRUE)
  structure(list(on = on), class = "retraction_fail_policy")
}

#' Evaluate a checked result against a fail policy
#'
#' Decides whether a [fail_policy()] is triggered by a result, for building
#' custom gates (the CLI, knit gate, and GitHub Action use it).
#'
#' @param res A [`retraction_result`][print.retraction_result].
#' @param policy A [fail_policy()].
#' @param n_errors Count of files that errored during the scan.
#' @return A list: `fail` (logical), `triggered` (character reasons), `counts`
#'   (the per-state counts), and `n_errors`.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' evaluate_policy(res, fail_policy("flagged"))$fail
#' }
#' @export
evaluate_policy <- function(res, policy, n_errors = 0L) {
  cnt <- result_counts(res)
  hits <- character(0)
  if ("flagged" %in% policy$on && cnt$flagged > 0) {
    hits <- c(hits, sprintf("%d flagged", cnt$flagged))
  }
  if ("possible" %in% policy$on && cnt$possible > 0) {
    hits <- c(hits, sprintf("%d possible", cnt$possible))
  }
  if ("unchecked" %in% policy$on && cnt$unchecked > 0) {
    hits <- c(hits, sprintf("%d unchecked", cnt$unchecked))
  }
  if ("error" %in% policy$on && n_errors > 0) {
    hits <- c(hits, sprintf("%d file error(s)", n_errors))
  }
  list(fail = length(hits) > 0, triggered = hits, counts = cnt, n_errors = n_errors)
}

#' One-line count summary printed by every workflow gate.
#' @noRd
print_state_counts <- function(cnt, n_errors = 0L) {
  cli::cli_text(
    "flagged: {cnt$flagged} | possible: {cnt$possible} | notice: {cnt$notice} | ",
    "clean: {cnt$clean} | unchecked: {cnt$unchecked} | file errors: {n_errors}"
  )
}
