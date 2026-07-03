# Gate a document render on retracted citations, so a manuscript cannot be knit
# while it still cites retracted work. (Roadmap A2.)
#
# Fails CLOSED: an unreadable input or a parse/network error is surfaced, not
# swallowed. A gate whose job is to catch problems must not pass silently when it
# could not check.

#' Fail (or warn) a knit when the document cites retracted work.
#'
#' Call from a setup chunk. Checks the document and/or its bibliography; if the
#' fail policy is triggered it aborts (or warns) the render.
#'
#' @param input Path to the document being knit. Defaults to
#'   `knitr::current_input()` when knitting.
#' @param bib Optional path(s) to bibliography files to check as well.
#' @param on Fail policy states (see [fail_policy()]): any of `"flagged"`,
#'   `"possible"`, `"unchecked"`, `"error"`. Default `"flagged"`.
#' @param action `"error"` (default; abort the render) or `"warn"`.
#' @param sources,offline Passed to [check_file()].
#' @return Invisibly, the combined `retraction_result`.
#' @examples
#' \dontrun{
#' retraction::retraction_knit_check(bib = "refs.bib",
#'                                   on = c("flagged", "unchecked"))
#' }
#' @export
retraction_knit_check <- function(input = NULL, bib = NULL,
                                  on = "flagged",
                                  action = c("error", "warn"),
                                  sources = getOption("retraction.sources", "xera"),
                                  offline = FALSE) {
  action <- match.arg(action)
  policy <- fail_policy(on)
  if (is.null(input) && requireNamespace("knitr", quietly = TRUE)) {
    input <- knitr::current_input()
  }
  paths <- unique(c(input, bib))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  emit <- function(msg) {
    if (action == "error") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    # A named bibliography that is not there is a checkable-thing we could not
    # check: surface it rather than proceeding as if clean.
    emit(sprintf("retraction: cannot read %s", paste(missing, collapse = ", ")))
  }
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    cli::cli_alert_info("retraction: nothing to check.")
    return(invisible(NULL))
  }

  scan <- retraction_scan(paths, on_missing = "skip",
                          sources = sources, offline = offline)
  res <- scan$result
  verdict <- evaluate_policy(res, policy, scan$n_errors)
  print_state_counts(verdict$counts, scan$n_errors)
  if (verdict$fail) {
    emit(sprintf("retraction: render gate triggered (%s)",
                 paste(verdict$triggered, collapse = "; ")))
  }
  invisible(res)
}
