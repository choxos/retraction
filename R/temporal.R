# Temporal analysis: was a retracted work's retraction before or after the
# document was written, and what share of a document's citations are retracted.
# (Roadmap C13, C16.)

#' Classify each citation relative to the document's date.
#'
#' Adds a `timing` column. The default date is the document's authoring date
#' (git commit or mtime), which is **not** the date a specific citation was
#' added, so the labels are deliberately conservative:
#' `"document_after_retraction"` means the document was written after the work
#' was retracted, not that the citation was knowingly added post-retraction. Pass
#' `citation_dates` for true citation-level timing.
#'
#' @param x A `retraction_result`.
#' @param document_date The document's date, `Date` or `YYYY-MM-DD`. Defaults to
#'   today; see [manuscript_date_of()] to derive it from a file.
#' @param citation_dates Optional named vector (`Date`/string) of per-citation
#'   dates, named by `id` or DOI. When supplied for a row, that date is used and
#'   the label becomes `"cited_after_retraction"` / `"cited_before_retraction"`.
#' @return `x` with an added character `timing` column (`NA` when no retraction
#'   date is known).
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' classify_timing(res, document_date = "2015-01-01")
#' }
#' @export
classify_timing <- function(x, document_date = Sys.Date(), citation_dates = NULL) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  doc <- as.Date(document_date)
  timing <- rep(NA_character_, nrow(x))
  for (i in seq_len(nrow(x))) {
    rdate <- x$retraction_date[i]
    if (is.na(rdate)) next
    cd <- NULL
    if (!is.null(citation_dates)) {
      cd <- citation_dates[[x$id[i]]] %||% citation_dates[[x$doi[i]]]
    }
    if (!is.null(cd) && !is.na(cd)) {
      timing[i] <- if (as.Date(cd) >= rdate) "cited_after_retraction" else "cited_before_retraction"
    } else {
      timing[i] <- if (doc >= rdate) "document_after_retraction" else "document_before_retraction"
    }
  }
  x$timing <- timing
  x
}

#' Best-effort authoring date of a manuscript file (git commit date, else mtime).
#'
#' @param path Path to the manuscript.
#' @return A `Date`, or today's date if nothing better is available.
#' @export
manuscript_date_of <- function(path) {
  d <- tryCatch({
    out <- suppressWarnings(system2("git", c("log", "-1", "--format=%cs", "--", shQuote(path)),
                                    stdout = TRUE, stderr = FALSE))
    if (length(out) && nzchar(out[1])) as.Date(out[1]) else NA
  }, error = function(e) NA)
  if (!is.na(d)) return(d)
  if (file.exists(path)) return(as.Date(file.info(path)$mtime))
  Sys.Date()
}

#' Retraction-exposure summary, with denominator diagnostics.
#'
#' A bare `flagged / n` headline is misleading when many rows are unchecked or
#' only "possible" (future-work review #7), so this reports the full breakdown
#' and two rates: per total, and per successfully-checked.
#'
#' @param x A `retraction_result`.
#' @return A named list: `n_total`, `n_checked`, `n_flagged`, `n_possible`,
#'   `n_unchecked`, `flagged_per_total`, `flagged_per_checked`.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' exposure_score(res)
#' }
#' @export
exposure_score <- function(x) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  cnt <- result_counts(x)
  n_total <- cnt$n
  n_unchecked <- cnt$unchecked
  n_checked <- n_total - n_unchecked
  rate <- function(num, den) if (den > 0) round(num / den, 4) else NA_real_
  list(
    n_total = n_total,
    n_checked = n_checked,
    n_flagged = cnt$flagged,
    n_possible = cnt$possible,
    n_unchecked = n_unchecked,
    flagged_per_total = rate(cnt$flagged, n_total),
    flagged_per_checked = rate(cnt$flagged, n_checked)
  )
}
