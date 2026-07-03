# RStudio addin: check the active document and jump to the first flagged line.
# (Roadmap A1.) Bound in inst/rstudio/addins.dcf.

#' RStudio addin: check retractions in the active source document.
#'
#' For a **saved** document, checks the file in place, so relative
#' `bibliography:` paths in the YAML header resolve. For an **unsaved** buffer it
#' can only scan inline identifiers (DOIs in the text), not an external
#' bibliography, and says so (future-work review #13). Prints the result and
#' moves the cursor to the first flagged citation when a location is known.
#'
#' @return Invisibly, the `retraction_result`.
#' @export
retraction_addin_check_active <- function() {
  need_pkg("rstudioapi", "for the RStudio addin")
  if (!rstudioapi::isAvailable()) {
    cli::cli_abort("This addin must be run inside RStudio.")
  }
  ctx <- rstudioapi::getSourceEditorContext()
  path <- ctx$path
  if (!nzchar(path)) {
    cli::cli_alert_info(
      "Document not saved: checking inline identifiers only, not an external bibliography."
    )
    path <- tempfile(fileext = ".txt")
    writeLines(ctx$contents, path)
  }
  res <- check_file(path, progress = FALSE)
  print(res)
  fl <- retracted(res, "flagged")
  if (nrow(fl) && !is.na(fl$location[1])) {
    line <- suppressWarnings(as.integer(fl$location[1]))
    if (!is.na(line)) {
      rstudioapi::setCursorPosition(rstudioapi::document_position(line, 1), id = ctx$id)
    }
  } else if (!nrow(fl)) {
    rstudioapi::showDialog("retraction", "No retracted citations found.")
  }
  invisible(res)
}
