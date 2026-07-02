#' retraction: Detect Retracted References in Documents and Bibliographies
#'
#' `retraction` scans manuscripts, bibliographies, and reference lists for
#' citations to retracted publications so that authors can avoid citing
#' retracted work. It reads a wide range of bibliographic and document formats,
#' extracts and normalizes identifiers, and checks them against retraction data.
#' The default data source is the Retraction Watch database served through the
#' XeraRetractionTracker API; Crossref and OpenAlex are available as additional,
#' reconcilable sources.
#'
#' The main entry points are [check_file()], [check_dois()], and [check_refs()].
#' See <https://choxos.github.io/retraction/> for details.
#'
#' @keywords internal
"_PACKAGE"
