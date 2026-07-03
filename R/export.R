# Export a checked result to a spreadsheet or data file. (Roadmap D18.)

#' Export a checked result to CSV, JSON, or Excel.
#'
#' Format is chosen from the file extension: `.csv`, `.json`, or `.xlsx`
#' (Excel requires the suggested `writexl` package).
#'
#' @param x A `retraction_result`.
#' @param path Output path; its extension selects the format.
#' @return Invisibly, `path`.
#' @examples
#' \dontrun{
#' res <- check_file("refs.bib")
#' export_result(res, "results.xlsx")
#' export_result(res, "results.csv")
#' }
#' @export
export_result <- function(x, path) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  df <- as.data.frame(x)
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv = utils::write.csv(df, path, row.names = FALSE, na = ""),
    json = jsonlite::write_json(df, path, auto_unbox = TRUE, na = "null",
                                pretty = TRUE),
    xlsx = {
      need_pkg("writexl", "to export to Excel")
      writexl::write_xlsx(df, path)
    },
    cli::cli_abort(c("Unsupported export format {.val {ext}}.",
                     "i" = "Use a path ending in .csv, .json, or .xlsx."))
  )
  cli::cli_alert_success("Wrote {nrow(df)} row{?s} -> {.file {path}}")
  invisible(path)
}
