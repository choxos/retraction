# Thin wrappers over the remote APIs. These return lightly-parsed structures
# (lists or data frames); adaptation to the uniform backend schema happens in
# backends.R. All of them inherit the non-throwing behavior of the HTTP layer.

## ---------------------------------------------------------------------------
## Xera / Retraction Watch
## ---------------------------------------------------------------------------

#' Search Xera by DOI (server-side substring, case-insensitive).
#'
#' Returns `NULL` when the request itself fails (transport error or non-2xx), so
#' callers can distinguish a failed lookup from a successful empty result (an
#' empty `list()`).
#' @noRd
xera_search_doi <- function(doi, per_page = 100L) {
  res <- xera_get("search/advanced", list(doi = doi, per_page = per_page))
  if (is.null(res)) return(NULL)
  pluck1(res, "items") %||% list()
}

#' Search the corpus by exact PubMed ID. NULL on request failure, else items.
#' @noRd
xera_search_pmid <- function(pmid, per_page = 20L) {
  res <- xera_get("search/advanced", list(pubmed_id = pmid, per_page = per_page))
  if (is.null(res)) return(NULL)
  pluck1(res, "items") %||% list()
}

#' Resolve a PMID to the retracted work's DOI via the Xera corpus.
#'
#' Authoritative (the corpus is Retraction Watch), with no OpenAlex dependency.
#' Returns the original paper's DOI only when the PMID matches a retracted work's
#' own PubMed ID; NULL when unmatched, when only a retraction-notice PMID matched
#' (citing the notice is not citing the work), or on failure.
#' @noRd
xera_pmid_to_doi <- function(pmid) {
  items <- xera_search_pmid(pmid)
  if (is.null(items) || !length(items)) return(NULL)
  pmid <- as.character(pmid)
  for (it in items) {
    if (identical(na_if_empty(pluck1(it, "original_paper_pubmed_id")), pmid)) {
      d <- normalize_doi(pluck1(it, "original_paper_doi") %||% NA_character_)
      if (!is.na(d)) return(d)
    }
  }
  NULL
}

#' Fetch a full Xera record by Retraction Watch record id.
#' @noRd
xera_paper <- function(record_id) {
  xera_get(paste0("papers/", utils::URLencode(as.character(record_id),
                                              reserved = TRUE)))
}

#' Fetch the corpus year range from the filter-options endpoint.
#' @return A list with `min` and `max`, or NULL.
#' @noRd
xera_year_range <- function() {
  res <- xera_get("papers/filter-options")
  yr <- pluck1(res, "year_range")
  if (is.null(yr)) return(NULL)
  list(min = as.integer(yr[["min"]] %||% NA), max = as.integer(yr[["max"]] %||% NA))
}

#' Export one slice of the papers table as a data frame (CSV backend).
#'
#' The export endpoint is rate-limited (20/min) and capped at 10,000 rows, so
#' callers slice by retraction year to stay within both limits.
#' @noRd
xera_export_slice <- function(year_from = NULL, year_to = NULL, limit = 10000L) {
  txt <- xera_get(
    "export/papers",
    list(format = "csv", limit = limit, year_from = year_from, year_to = year_to),
    parse = "text",
    # Steady, no-burst rate (about 15 requests/minute) to stay under the
    # export endpoint's 20/minute cap without tripping 429 backoffs.
    throttle = list(capacity = 1, fill_time_s = 4)
  )
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  out <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = TRUE,
                    colClasses = "character", na.strings = c("", "NA")),
    error = function(e) NULL
  )
  out
}

# Columns the snapshot needs, matching the export CSV.
SNAPSHOT_COLS <- c(
  "record_id", "title", "original_paper_doi", "retraction_doi",
  "original_paper_pubmed_id", "retraction_pubmed_id", "journal",
  "publisher", "author", "original_paper_date", "retraction_date",
  "retraction_nature", "reason", "subject", "country", "citation_count",
  "is_open_access"
)

#' Build a snapshot-shaped data frame from `/papers` list items.
#' @noRd
papers_items_to_df <- function(items) {
  if (!length(items)) return(NULL)
  cols <- lapply(SNAPSHOT_COLS, function(k) {
    vapply(items, function(it) na_if_empty(pluck1(it, k)) %||% NA_character_,
           character(1))
  })
  names(cols) <- SNAPSHOT_COLS
  as.data.frame(cols, stringsAsFactors = FALSE)
}

#' Fetch a full retraction year via the paginated `/papers` endpoint.
#'
#' Used as a fallback when a single year exceeds the 10,000-row export cap, so
#' no records are silently truncated. `/papers` is not rate-limited and paginates
#' with `page`/`per_page`.
#' @noRd
xera_papers_year <- function(year, per_page = 100L, max_pages = 10000L) {
  page <- 1L; frames <- list()
  repeat {
    res <- xera_get("papers", list(year_from = year, year_to = year,
                                   per_page = per_page, page = page))
    items <- pluck1(res, "items")
    if (is.null(items) || !length(items)) break
    frames[[length(frames) + 1L]] <- papers_items_to_df(items)
    total_pages <- suppressWarnings(as.integer(pluck1(res, "total_pages") %||% 1L))
    if (is.na(total_pages) || page >= total_pages || page >= max_pages) break
    page <- page + 1L
  }
  if (length(frames)) do.call(rbind, frames) else NULL
}

## ---------------------------------------------------------------------------
## Crossref
## ---------------------------------------------------------------------------

#' Fetch a Crossref work by DOI.
#' @return The `message` list, or NULL.
#' @noRd
crossref_work <- function(doi) {
  url <- paste0("https://api.crossref.org/works/",
                utils::URLencode(doi, reserved = TRUE))
  res <- http_get_json(url, query = list(mailto = retraction_mailto()))
  pluck1(res, "message")
}

## ---------------------------------------------------------------------------
## OpenAlex
## ---------------------------------------------------------------------------

OPENALEX_SELECT <- "id,doi,display_name,type,publication_year,is_retracted,ids"

#' Fetch an OpenAlex work by DOI.
#' @noRd
openalex_by_doi <- function(doi) {
  url <- paste0("https://api.openalex.org/works/doi:",
                utils::URLencode(doi, reserved = TRUE))
  http_get_json(url, query = list(select = OPENALEX_SELECT,
                                  mailto = retraction_mailto()))
}

#' Fetch an OpenAlex work by PMID (also used to resolve PMID -> DOI).
#' @noRd
openalex_by_pmid <- function(pmid) {
  url <- paste0("https://api.openalex.org/works/pmid:", utils::URLencode(pmid))
  http_get_json(url, query = list(select = OPENALEX_SELECT,
                                  mailto = retraction_mailto()))
}

#' Resolve a PMID to a normalized DOI via OpenAlex, or `NA`.
#'
#' A fallback used only when the Xera corpus has no record for a PMID (see
#' `xera_pmid_to_doi()`), to obtain a DOI so the other DOI-only sources can be
#' queried.
#' @noRd
resolve_pmid_to_doi <- function(pmid) {
  work <- openalex_by_pmid(pmid)
  doi <- pluck1(work, "doi")
  if (is.null(doi)) return(NA_character_)
  normalize_doi(doi)
}
