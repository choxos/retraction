# Small reporting and diagnostic helpers built on the result and snapshot.

#' Write a shields.io endpoint badge for a checked result.
#'
#' Produces a JSON file suitable for a
#' [shields.io endpoint badge](https://shields.io/endpoint), showing the number
#' of retracted citations. Point a badge URL at the hosted file for a README or a
#' paper's landing page.
#'
#' @param x A [`retraction_result`][print.retraction_result].
#' @param path Output path for the JSON. Defaults to `"retraction-badge.json"`.
#' @return Invisibly, `path`.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' badge_json(res, tempfile(fileext = ".json"))
#' }
#' @export
badge_json <- function(x, path = "retraction-badge.json") {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  n <- sum(x$is_retracted %in% TRUE)
  jsonlite::write_json(
    list(schemaVersion = 1L, label = "retracted citations",
         message = as.character(n), color = if (n > 0L) "red" else "brightgreen"),
    path, auto_unbox = TRUE
  )
  invisible(path)
}

#' Report the local snapshot's data version and freshness
#'
#' Which retraction-database version an offline check runs against, for
#' reproducible checks.
#'
#' @return Invisibly, a list with `path`, `records`, `synced_at`, and
#'   `newest_retraction`; or `NULL` if no snapshot exists. Also prints a summary.
#' @examples
#' \dontrun{
#' snapshot_info()
#' }
#' @export
snapshot_info <- function() {
  snap <- load_snapshot()
  if (is.null(snap)) {
    cli::cli_alert_info("No local snapshot. Run {.run retraction_sync()} to build one.")
    return(invisible(NULL))
  }
  synced <- attr(snap, "synced_at")
  newest <- suppressWarnings(max(as.Date(snap$retraction_date), na.rm = TRUE))
  info <- list(
    path = snapshot_path(),
    records = nrow(snap),
    synced_at = if (is.null(synced)) NA else as.POSIXct(synced),
    newest_retraction = if (is.finite(newest)) newest else as.Date(NA)
  )
  synced_txt <- if (is.na(info$synced_at)) "unknown" else format(as.Date(info$synced_at))
  newest_txt <- if (is.na(info$newest_retraction)) "unknown" else format(info$newest_retraction)
  cli::cli_alert_info(
    "Snapshot: {info$records} records; newest {newest_txt}; synced {synced_txt}."
  )
  invisible(info)
}

#' Explain the verdict for each reference in a result
#'
#' A human-readable sentence per row: what matched, on which identifier, at what
#' confidence, which sources confirmed, and any disagreement.
#'
#' @param x A [`retraction_result`][print.retraction_result].
#' @param rows Optional integer or logical row selector; defaults to all rows.
#' @return A [tibble][tibble::tibble] with `id`, `status`, and `explanation`.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' explain_result(res)
#' }
#' @export
explain_result <- function(x, rows = NULL) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  idx <- if (is.null(rows)) {
    seq_len(nrow(x))
  } else if (is.logical(rows)) {
    which(rows)
  } else {
    as.integer(rows)
  }
  explanation <- vapply(idx, function(i) {
    r <- x[i, ]
    if (!isTRUE(r$matched)) {
      if (r$status %in% "unchecked") {
        return("Could not be checked: every selected source failed or was unavailable.")
      }
      return("No matching retraction record was found; treated as clean.")
    }
    what <- status_label(r$status)
    how <- switch(
      r$matched_on %||% "",
      original_doi = "matched by exact DOI to the retracted work",
      retraction_doi = "matched the retraction notice's DOI (cites the notice, not the work)",
      pmid = "matched by PMID",
      title = sprintf("matched by %s title similarity",
                      if (identical(r$match_type, "title_exact")) "exact-metadata" else "fuzzy"),
      "matched"
    )
    src <- if (!is.na(r$sources)) sprintf("; confirmed by %s", r$sources) else ""
    dis <- if (isTRUE(r$disagreement) && !is.na(r$disagreeing)) {
      sprintf("; disagreement from %s", r$disagreeing)
    } else {
      ""
    }
    verdict <- if (isTRUE(r$is_retracted)) "flagged" else "recorded as a notice, not flagged"
    sprintf("%s: %s (confidence %.2f)%s%s; %s.",
            what, how, r$confidence %||% 0, src, dis, verdict)
  }, character(1))
  tibble::tibble(id = x$id[idx], status = x$status[idx], explanation = explanation)
}

#' Summarize cross-source (dis)agreement in a result
#'
#' The rows where the selected sources disagreed, with which confirmed and which
#' dissented. Meaningful only when more than one source was queried.
#'
#' @param x A [`retraction_result`][print.retraction_result].
#' @return A [tibble][tibble::tibble] of the disagreeing rows (`id`,
#'   `identifier`, `status`, `confirmed_by`, `dissenting`); zero rows when all
#'   sources agree.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0",
#'                   sources = c("xera", "openalex"))
#' compare_sources(res)
#' }
#' @export
compare_sources <- function(x) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  keep <- x$disagreement %in% TRUE
  # Fall back to PMID (or id) so a PMID-only disagreement is still identifiable.
  ident <- ifelse(!is.na(x$doi[keep]), x$doi[keep],
                  ifelse(!is.na(x$pmid[keep]), paste0("pmid:", x$pmid[keep]),
                         x$id[keep]))
  tibble::tibble(
    id = x$id[keep],
    identifier = ident,
    status = x$status[keep],
    confirmed_by = x$sources[keep],
    dissenting = x$disagreeing[keep]
  )
}
