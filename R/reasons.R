# Bucket free-text retraction reasons into a small taxonomy. (Roadmap E25.)
#
# Retraction Watch reasons are free text and often list several causes, so this
# is deliberately COARSE and ENGLISH-ONLY (future-work review #11 / P7). The
# primary bucket is lossy (first match wins); use `reason_buckets()` for the full
# multi-label set.

REASON_BUCKETS <- c(
  misconduct = "fabricat|falsif|manipulat|misconduct|fake|forged|data integrity",
  plagiarism = "plagiar|duplicat|overlap|self.?citation|paper mill",
  error      = "error|honest|reproduc|irreproduc|unreliable|flawed",
  ethical    = "ethic|consent|irb|approval|legal|copyright|privacy",
  process    = "peer.?review|editor|publisher|authorship|withdrawal|investigation"
)

#' All matching reason buckets (multi-label).
#'
#' @param reason A single free-text reason string.
#' @return A character vector of every matching bucket (possibly several), or
#'   `"other"` if none match.
#' @examples
#' reason_buckets("Fabrication of data; Authorship disputes")
#' @export
reason_buckets <- function(reason) {
  r <- tolower(na_if_empty(reason) %||% "")
  if (!nzchar(r)) return("other")
  hit <- names(REASON_BUCKETS)[vapply(REASON_BUCKETS,
                                      function(p) grepl(p, r, perl = TRUE), logical(1))]
  if (length(hit)) hit else "other"
}

#' Primary (lossy) reason bucket: the first matching bucket.
#'
#' Coarse and English-only; for reasons with several causes it keeps only the
#' first match in bucket order (misconduct > plagiarism > error > ethical >
#' process). Use [reason_buckets()] to keep all causes.
#'
#' @param reason A character vector of reason strings.
#' @return A character vector of primary buckets.
#' @examples
#' primary_reason_bucket(c("Fabrication of data", "Duplicate publication", "Unknown"))
#' @export
primary_reason_bucket <- function(reason) {
  vapply(reason, function(r) reason_buckets(r)[1], character(1), USE.NAMES = FALSE)
}

#' Tabulate primary reason buckets across a checked result.
#'
#' @param x A `retraction_result`.
#' @return A named integer table of primary-bucket counts over the matched rows.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' reason_summary(res)
#' }
#' @export
reason_summary <- function(x) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  matched <- x[x$matched %in% TRUE & !is.na(x$reason), , drop = FALSE]
  sort(table(primary_reason_bucket(matched$reason)), decreasing = TRUE)
}
