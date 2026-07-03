# Launcher for the bundled Shiny triage app. (Roadmap A6.)

#' Launch the retraction triage Shiny app
#'
#' Opens a small web app to upload a bibliography or document and browse the
#' flagged, possible, and clean references interactively.
#'
#' @param ... Passed to [shiny::runApp()].
#' @return Runs the app; does not return a value. Requires the suggested shiny
#'   and DT packages.
#' @examples
#' \dontrun{
#' retraction_app()
#' }
#' @export
retraction_app <- function(...) {
  need_pkg("shiny", "to launch the triage app")
  need_pkg("DT", "to launch the triage app")
  app_dir <- system.file("shiny", package = "retraction")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    cli::cli_abort("The bundled Shiny app was not found in the installed package.")
  }
  shiny::runApp(app_dir, ...)
}
