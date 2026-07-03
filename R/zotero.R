# Scan a Zotero library for retracted references by reading its SQLite database.
# (Roadmap A5.) For a library that is open in Zotero the database may be locked;
# in that case export via Better BibTeX and use check_file() on the .bib instead.

#' Default Zotero SQLite path for the current platform.
#' @noRd
zotero_default_db <- function() {
  home <- path.expand("~")
  candidates <- c(
    file.path(home, "Zotero", "zotero.sqlite"),
    file.path(home, "Library", "Application Support", "Zotero", "zotero.sqlite")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[1] else candidates[1]
}

#' Pivot Zotero's long-format field rows into one row per item (pure).
#' @param rows A data frame with columns `itemID`, `field`, `value`.
#' @return A data frame with `doi`, `title`, `year` columns.
#' @noRd
zotero_items_to_df <- function(rows) {
  if (!nrow(rows)) {
    return(data.frame(doi = character(), title = character(), year = character(),
                      stringsAsFactors = FALSE))
  }
  by_item <- split(rows, rows$itemID)
  getf <- function(g, fn) {
    v <- g$value[g$field == fn]
    if (length(v)) as.character(v[[1]]) else NA_character_
  }
  do.call(rbind, lapply(by_item, function(g) {
    data.frame(
      doi = getf(g, "DOI"),
      title = getf(g, "title"),
      year = substr(getf(g, "date"), 1, 4),
      stringsAsFactors = FALSE
    )
  }))
}

#' Check a Zotero library for retracted references
#'
#' Reads a Zotero SQLite database directly (read-only) and checks every item's
#' DOI and title. If Zotero has the library open the database may be locked; in
#' that case export the library via Better BibTeX and use [check_file()].
#'
#' @param path Path to `zotero.sqlite`, or the directory containing it. Defaults
#'   to the standard per-user location.
#' @inheritParams check_dois
#' @return A [`retraction_result`][print.retraction_result] tibble. Requires the
#'   suggested DBI and RSQLite packages.
#' @examples
#' \dontrun{
#' check_zotero()
#' }
#' @export
check_zotero <- function(path = zotero_default_db(),
                         sources = getOption("retraction.sources", "xera"),
                         offline = FALSE,
                         flag_nature = c("Retraction", "Expression of Concern"),
                         allow_fuzzy = TRUE, resolve_ids = TRUE, progress = TRUE,
                         strict = FALSE) {
  need_pkg("DBI", "to read a Zotero database")
  need_pkg("RSQLite", "to read a Zotero database")
  db <- if (dir.exists(path)) file.path(path, "zotero.sqlite") else path
  if (!file.exists(db)) {
    cli::cli_abort(c("No Zotero database found at {.file {db}}.",
                     "i" = "Pass the path to your {.file zotero.sqlite}."))
  }
  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO),
    error = function(e) {
      cli::cli_abort(c(
        "Could not open the Zotero database (it may be locked by Zotero).",
        "i" = "Close Zotero, or export via Better BibTeX and use {.fn check_file}."
      ))
    }
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, paste(
    "SELECT d.itemID AS itemID, f.fieldName AS field, v.value AS value",
    "FROM itemData d",
    "JOIN itemDataValues v ON v.valueID = d.valueID",
    "JOIN fieldsCombined f ON f.fieldID = d.fieldID",
    "WHERE f.fieldName IN ('DOI', 'title', 'date')"
  ))
  df <- zotero_items_to_df(rows)
  if (!nrow(df)) {
    cli::cli_warn("No DOIs or titles found in the Zotero library.")
    return(new_retraction_result(list()))
  }
  check_refs(df, sources = sources, offline = offline, flag_nature = flag_nature,
             allow_fuzzy = allow_fuzzy, resolve_ids = resolve_ids,
             progress = progress, strict = strict)
}
