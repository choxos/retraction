# The `retraction_result` object: a classed tibble with one row per reference,
# plus print/summary/coercion methods.

# Canonical column order.
RESULT_COLS <- c(
  "id", "input_type", "query", "doi", "pmid", "matched", "status",
  "is_retracted", "confidence", "match_type", "matched_on", "nature",
  "record_id", "matched_title", "journal", "retraction_date",
  "days_since_retraction", "reason", "sources", "disagreement",
  "source_file", "location"
)

#' Assemble result rows into a classed tibble.
#' @noRd
new_retraction_result <- function(rows) {
  tbl <- rows_to_tibble(rows)
  class(tbl) <- c("retraction_result", class(tbl))
  tbl
}

#' @noRd
rows_to_tibble <- function(rows) {
  if (!length(rows)) {
    tbl <- tibble::tibble(
      id = character(), input_type = character(), query = character(),
      doi = character(), pmid = character(), matched = logical(),
      status = character(), is_retracted = logical(), confidence = numeric(),
      match_type = character(), matched_on = character(), nature = character(),
      record_id = character(), matched_title = character(), journal = character(),
      retraction_date = as.Date(character()),
      days_since_retraction = integer(), reason = character(),
      sources = character(), disagreement = logical(),
      source_file = character(), location = character()
    )
    return(tbl)
  }

  chr <- function(nm) {
    vapply(rows, function(r) {
      v <- r[[nm]]
      if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v)[1]
    }, character(1))
  }
  lgl <- function(nm) vapply(rows, function(r) isTRUE(r[[nm]]), logical(1))
  num <- function(nm) {
    vapply(rows, function(r) {
      v <- r[[nm]]
      if (is.null(v) || length(v) == 0L) NA_real_ else as.numeric(v)[1]
    }, numeric(1))
  }
  intg <- function(nm) {
    vapply(rows, function(r) {
      v <- r[[nm]]
      if (is.null(v) || length(v) == 0L) NA_integer_ else as.integer(v)[1]
    }, integer(1))
  }
  datec <- function(nm) {
    as.Date(vapply(rows, function(r) {
      v <- r[[nm]]
      if (is.null(v) || length(v) == 0L || is.na(v)) NA_character_
      else as.character(as.Date(v))
    }, character(1)))
  }

  tibble::tibble(
    id = chr("id"), input_type = chr("input_type"), query = chr("query"),
    doi = chr("doi"), pmid = chr("pmid"), matched = lgl("matched"),
    status = chr("status"), is_retracted = lgl("is_retracted"),
    confidence = num("confidence"), match_type = chr("match_type"),
    matched_on = chr("matched_on"), nature = chr("nature"),
    record_id = chr("record_id"), matched_title = chr("matched_title"),
    journal = chr("journal"), retraction_date = datec("retraction_date"),
    days_since_retraction = intg("days_since_retraction"), reason = chr("reason"),
    sources = chr("sources"), disagreement = lgl("disagreement"),
    source_file = chr("source_file"), location = chr("location")
  )
}

#' Subset of references flagged as retracted (or otherwise notable).
#'
#' @param x A `retraction_result`.
#' @param which "flagged" (default; high-confidence flagged citations),
#'   "possible" (matched to a flagged record but below the confidence
#'   threshold), or "all_matched".
#' @return A `retraction_result` with the selected rows.
#' @export
retracted <- function(x, which = c("flagged", "possible", "all_matched")) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  which <- match.arg(which)
  keep <- switch(
    which,
    flagged = x$is_retracted %in% TRUE,
    possible = x$matched %in% TRUE & !(x$is_retracted %in% TRUE) &
      x$status %in% c("retracted", "expression_of_concern"),
    all_matched = x$matched %in% TRUE
  )
  out <- x[keep, , drop = FALSE]
  if (!inherits(out, "retraction_result")) {
    class(out) <- c("retraction_result", class(out))
  }
  out
}

#' @noRd
result_counts <- function(x) {
  flagged <- sum(x$is_retracted %in% TRUE)
  possible <- sum(x$matched %in% TRUE & !(x$is_retracted %in% TRUE) &
                    x$status %in% c("retracted", "expression_of_concern"))
  notice <- sum(x$status %in% c("correction", "reinstated"))
  unchecked <- sum(x$status %in% "unchecked")
  clean <- nrow(x) - flagged - possible - notice - unchecked
  list(n = nrow(x), flagged = flagged, possible = possible, notice = notice,
       clean = clean, unchecked = unchecked)
}

#' Print a retraction result.
#' @param x A `retraction_result`.
#' @param ... Unused.
#' @return The input `x`, invisibly.
#' @export
print.retraction_result <- function(x, ...) {
  cnt <- result_counts(x)
  cli::cli_h1("retraction: {cnt$n} reference{?s} checked")
  if (cnt$flagged) cli::cli_alert_danger("{cnt$flagged} retracted or flagged")
  if (cnt$possible) cli::cli_alert_warning("{cnt$possible} possible (low confidence)")
  if (cnt$notice) cli::cli_alert_info("{cnt$notice} other notice (correction or reinstatement)")
  cli::cli_alert_success("{cnt$clean} clean")
  if (cnt$unchecked) cli::cli_alert("{cnt$unchecked} could not be checked")

  flagged <- retracted(x, "flagged")
  if (nrow(flagged)) {
    cli::cli_h2("Flagged citations")
    for (i in seq_len(nrow(flagged))) {
      r <- flagged[i, ]
      loc <- if (!is.na(r$source_file)) {
        sprintf(" [%s%s]", basename(r$source_file),
                if (!is.na(r$location)) paste0(":", r$location) else "")
      } else {
        ""
      }
      when <- if (!is.na(r$retraction_date)) {
        sprintf("retracted %s", format(r$retraction_date))
      } else {
        "retraction date unknown"
      }
      ttl <- if (is.na(r$matched_title)) "(no title)" else r$matched_title
      cli::cli_li("{.strong {r$id}}{loc}: {ttl} ({when}; source: {r$sources})")
    }
    cli::cli_end()
  }
  possible <- retracted(x, "possible")
  if (nrow(possible)) {
    cli::cli_h2("Possible matches (verify manually)")
    for (i in seq_len(nrow(possible))) {
      r <- possible[i, ]
      ttl <- if (is.na(r$matched_title)) "(no title)" else r$matched_title
      cli::cli_li("{.strong {r$id}}: {ttl} (confidence {r$confidence})")
    }
    cli::cli_end()
  }
  cli::cli_text("")
  cli::cli_alert_info("Full table: {.code as.data.frame(x)} or {.code tibble::as_tibble(x)}.")
  invisible(x)
}

#' Summarize a retraction result as a status tally.
#' @param object A `retraction_result`.
#' @param ... Unused.
#' @return A tibble with one row per status category and its count.
#' @export
summary.retraction_result <- function(object, ...) {
  cnt <- result_counts(object)
  tibble::tibble(
    metric = c("references", "flagged", "possible", "other_notice",
               "clean", "unchecked"),
    n = c(cnt$n, cnt$flagged, cnt$possible, cnt$notice, cnt$clean,
          cnt$unchecked)
  )
}

#' Coerce a retraction result to a plain tibble.
#' @param x A `retraction_result`.
#' @param ... Unused.
#' @return A plain [tibble::tibble].
#' @exportS3Method tibble::as_tibble
as_tibble.retraction_result <- function(x, ...) {
  class(x) <- setdiff(class(x), "retraction_result")
  x
}

#' Coerce a retraction result to a data frame.
#' @param x A `retraction_result`.
#' @param row.names,optional Passed to [base::as.data.frame()].
#' @param ... Passed to [base::as.data.frame()].
#' @return A base [data.frame][base::data.frame].
#' @export
as.data.frame.retraction_result <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- setdiff(class(x), "retraction_result")
  as.data.frame(x, row.names = row.names, optional = optional, ...)
}
