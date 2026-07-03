# Systematic-review helper: flag retracted studies among a review's included set.
# A retracted included trial can invalidate a pooled estimate. (Roadmap E22.)
#
# This is a *helper*, not a full review data model: it dedupes identifiers and
# reports proper denominators, but does not track screening stage, multiple
# reports of one study, or trial-registration IDs. Those belong in a later,
# richer model.

#' Check the included studies of a systematic review or meta-analysis.
#'
#' @param ids A character vector of DOIs or PMIDs for the included studies.
#'   Duplicates are collapsed and counted once.
#' @param sources,offline,flag_nature,... Passed to [check_dois()].
#' @return A `retraction_review` (a `retraction_result` with `n_included`,
#'   `n_unique`, `n_checked`, `n_unchecked`, `n_possible`, and `n_retracted`
#'   attributes).
#' @examples
#' \donttest{
#' check_included_studies(c("10.1016/S0140-6736(97)11096-0",
#'                          "10.1136/bmj.331.7531.1512"))
#' }
#' @export
check_included_studies <- function(ids, sources = getOption("retraction.sources", "xera"),
                                   offline = FALSE,
                                   flag_nature = c("Retraction", "Expression of Concern"),
                                   ...) {
  ids_raw <- as.character(ids)
  ids_unique <- unique(ids_raw[!is.na(ids_raw) & nzchar(trimws(ids_raw))])
  res <- check_dois(ids_unique, sources = sources, offline = offline,
                    flag_nature = flag_nature, ...)
  cnt <- result_counts(res)
  attr(res, "n_included") <- length(ids_raw)
  attr(res, "n_unique") <- length(ids_unique)
  attr(res, "n_checked") <- cnt$n - cnt$unchecked
  attr(res, "n_unchecked") <- cnt$unchecked
  attr(res, "n_possible") <- cnt$possible
  attr(res, "n_retracted") <- cnt$flagged
  class(res) <- c("retraction_review", class(res))
  res
}

#' Print a systematic-review check, leading with denominators.
#' @param x A `retraction_review`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @exportS3Method print retraction_review
print.retraction_review <- function(x, ...) {
  nr <- attr(x, "n_retracted") %||% 0L
  nu <- attr(x, "n_unique") %||% nrow(x)
  ni <- attr(x, "n_included") %||% nu
  unchecked <- attr(x, "n_unchecked") %||% 0L
  if (nr > 0L) {
    cli::cli_alert_danger(
      "{nr}/{nu} included stud{?y/ies} retracted or flagged. Pooled estimates may be affected."
    )
  } else {
    cli::cli_alert_success("0/{nu} included studies retracted.")
  }
  if (ni != nu) cli::cli_alert_info("{ni - nu} duplicate identifier{?s} collapsed.")
  if (unchecked > 0L) {
    cli::cli_alert_warning("{unchecked} stud{?y/ies} could not be checked; treat as unverified.")
  }
  NextMethod()
}
