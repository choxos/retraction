# Public entry points and the shared checking engine.

#' Build a normalized reference from raw fields.
#' @noRd
as_reference <- function(query = NA, doi = NA, pmid = NA, title = NA,
                         author = NA, year = NA, id = NULL, input_type = NULL,
                         source_file = NA, location = NA) {
  ndoi <- normalize_doi(doi)[1]
  npmid <- normalize_pmid(pmid)[1]
  q <- na_if_empty(query)
  if (is.na(ndoi) && is.na(npmid) && !is.na(q)) {
    if (looks_like_doi(q)) ndoi <- normalize_doi(q)[1]
    else if (looks_like_pmid(q)) npmid <- normalize_pmid(q)[1]
  }
  ntitle <- na_if_empty(title)
  it <- input_type %||% (
    if (!is.na(ndoi)) "doi"
    else if (!is.na(npmid)) "pmid"
    else if (!is.na(ntitle)) "title"
    else NA_character_
  )
  idv <- id %||% (
    if (!is.na(ndoi)) ndoi
    else if (!is.na(npmid)) npmid
    else if (!is.na(ntitle)) ntitle
    else q
  )
  list(
    id = idv %||% NA_character_,
    input_type = it,
    query = q %||% ndoi %||% npmid %||% ntitle,
    doi = ndoi, pmid = npmid, title = ntitle,
    author = na_if_empty(author),
    year = year_of(as.character(year %||% NA_character_)),
    source_file = na_if_empty(source_file),
    location = if (is.null(location) || is.na(location)) NA_character_ else as.character(location)
  )
}

#' Case-insensitive column detection: exact match first, then prefix.
#' @noRd
detect_col <- function(names, candidates) {
  low <- tolower(names)
  for (c in candidates) {
    hit <- which(low == c)
    if (length(hit)) return(names[hit[1L]])
  }
  for (c in candidates) {
    # Prefix match, but only at a separator boundary so "doix" does not match
    # "doi" while "doi_url" still does.
    hit <- which(grepl(paste0("^", c, "([^[:alnum:]]|$)"), low))
    if (length(hit)) return(names[hit[1L]])
  }
  NULL
}

#' Shared checking engine.
#' @noRd
run_checks <- function(refs, sources, offline, flag_nature, allow_fuzzy,
                       resolve_ids, progress = TRUE) {
  sources <- resolve_sources(sources)
  snap <- NULL
  if (isTRUE(offline)) {
    snap <- load_snapshot()
    if (is.null(snap)) {
      cli::cli_abort(c(
        "No local retraction snapshot was found.",
        "i" = "Run {.run retraction_sync()} to download one, or pass {.code offline = FALSE}."
      ))
    }
    snapshot_freshness_check(snap)
  }
  ctx <- list(offline = offline, snapshot = snap, allow_fuzzy = allow_fuzzy,
              resolve_ids = resolve_ids, flag_nature = flag_nature)

  n <- length(refs)
  rows <- vector("list", n)
  if (isTRUE(progress) && n > 1L && interactive()) {
    cli::cli_progress_bar("Checking references", total = n, clear = TRUE)
    for (i in seq_len(n)) {
      rows[[i]] <- match_reference(refs[[i]], sources, ctx)
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  } else {
    for (i in seq_len(n)) rows[[i]] <- match_reference(refs[[i]], sources, ctx)
  }
  new_retraction_result(rows)
}

#' Check a set of DOIs or PMIDs for retraction
#'
#' @param x A character vector of DOIs and/or PMIDs (mixed is fine; each value
#'   is auto-detected).
#' @param sources Sources to query. A character vector of names from
#'   [list_backends()], or `"all"`. Defaults to
#'   `getOption("retraction.sources", "xera")`.
#' @param offline If `TRUE`, match against the local snapshot built by
#'   [retraction_sync()] instead of querying the network.
#' @param flag_nature Notice labels that count as "flag this citation". Defaults
#'   to Retraction and Expression of Concern.
#' @param allow_fuzzy Allow fuzzy title matching for references without a usable
#'   identifier.
#' @param resolve_ids If `TRUE`, resolve PMID-only references to a DOI via
#'   OpenAlex before matching.
#' @param progress Show a progress bar in interactive sessions.
#' @return A [`retraction_result`][print.retraction_result] tibble.
#' @examples
#' \donttest{
#' check_dois(c("10.1016/S0140-6736(97)11096-0", "10.1126/science.aac4716"))
#' }
#' @export
check_dois <- function(x, sources = getOption("retraction.sources", "xera"),
                       offline = FALSE, flag_nature = c("Retraction", "Expression of Concern"),
                       allow_fuzzy = TRUE, resolve_ids = TRUE, progress = TRUE) {
  x <- as_chr(x)
  refs <- lapply(x, function(q) as_reference(query = q))
  run_checks(refs, sources, offline, flag_nature, allow_fuzzy, resolve_ids, progress)
}

#' Check a data frame of references for retraction
#'
#' @param data A data frame of references.
#' @param doi_col,pmid_col,title_col,author_col,year_col Column names. When
#'   `NULL` (default) they are auto-detected from common names.
#' @inheritParams check_dois
#' @return A [`retraction_result`][print.retraction_result] tibble.
#' @examples
#' \donttest{
#' df <- data.frame(doi = "10.1016/S0140-6736(97)11096-0", title = "Ileal-lymphoid...")
#' check_refs(df)
#' }
#' @export
check_refs <- function(data, doi_col = NULL, pmid_col = NULL, title_col = NULL,
                       author_col = NULL, year_col = NULL,
                       sources = getOption("retraction.sources", "xera"),
                       offline = FALSE,
                       flag_nature = c("Retraction", "Expression of Concern"),
                       allow_fuzzy = TRUE, resolve_ids = TRUE, progress = TRUE) {
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  nm <- names(data)
  for (arg in c("doi_col", "pmid_col", "title_col", "author_col", "year_col")) {
    v <- get(arg)
    if (!is.null(v) && !(v %in% nm)) {
      cli::cli_abort(c(
        "Column {.val {v}} (passed as {.arg {arg}}) is not in {.arg data}.",
        "i" = "Available columns: {.val {nm}}."
      ))
    }
  }
  doi_col <- doi_col %||% detect_col(nm, c("doi"))
  pmid_col <- pmid_col %||% detect_col(nm, c("pmid", "pubmed_id", "pubmed"))
  title_col <- title_col %||% detect_col(nm, c("title"))
  author_col <- author_col %||% detect_col(nm, c("author", "authors"))
  year_col <- year_col %||% detect_col(nm, c("year", "issued", "date"))

  getval <- function(col, i) if (!is.null(col)) data[[col]][i] else NA
  refs <- lapply(seq_len(nrow(data)), function(i) {
    as_reference(
      doi = getval(doi_col, i), pmid = getval(pmid_col, i),
      title = getval(title_col, i), author = getval(author_col, i),
      year = getval(year_col, i), id = paste0("row", i)
    )
  })
  run_checks(refs, sources, offline, flag_nature, allow_fuzzy, resolve_ids, progress)
}

#' Check a document or bibliography file for retracted references
#'
#' Reads a file, extracts references and identifiers, and checks them. Supported
#' formats: BibTeX/BibLaTeX (`.bib`), CSL-JSON (`.json`), RIS (`.ris`), EndNote
#' XML and JATS XML (`.xml`), Word (`.docx`), PDF (`.pdf`), and any text-like
#' document (`.Rmd`, `.qmd`, `.tex`, `.md`, `.txt`, `.html`), from which DOIs are
#' scraped.
#'
#' @param path One or more file paths.
#' @param format Force a parser (e.g. "bib", "ris", "csljson", "endnote",
#'   "jats", "docx", "pdf", "text"). When `NULL`, inferred from the extension
#'   and, for `.xml`, the root element.
#' @inheritParams check_dois
#' @return A [`retraction_result`][print.retraction_result] tibble.
#' @examples
#' \donttest{
#' bib <- system.file("extdata", "example.bib", package = "retraction")
#' if (nzchar(bib)) check_file(bib)
#' }
#' @export
check_file <- function(path, format = NULL,
                       sources = getOption("retraction.sources", "xera"),
                       offline = FALSE,
                       flag_nature = c("Retraction", "Expression of Concern"),
                       allow_fuzzy = TRUE, resolve_ids = TRUE, progress = TRUE) {
  paths <- as_chr(path)
  refs <- unlist(lapply(paths, function(p) parse_input(p, format = format)),
                 recursive = FALSE)
  if (!length(refs)) {
    cli::cli_warn("No references or identifiers were found in the input.")
    return(new_retraction_result(list()))
  }
  run_checks(refs, sources, offline, flag_nature, allow_fuzzy, resolve_ids, progress)
}

#' Check a BibTeX or BibLaTeX bibliography for retracted references
#'
#' A thin wrapper around [check_file()] that forces the BibTeX parser.
#'
#' @param path Path to a `.bib` file.
#' @inheritParams check_dois
#' @return A [`retraction_result`][print.retraction_result] tibble.
#' @examples
#' \donttest{
#' bib <- system.file("extdata", "example.bib", package = "retraction")
#' if (nzchar(bib)) check_bib(bib)
#' }
#' @export
check_bib <- function(path, sources = getOption("retraction.sources", "xera"),
                      offline = FALSE,
                      flag_nature = c("Retraction", "Expression of Concern"),
                      allow_fuzzy = TRUE, resolve_ids = TRUE, progress = TRUE) {
  check_file(path, format = "bib", sources = sources, offline = offline,
             flag_nature = flag_nature, allow_fuzzy = allow_fuzzy,
             resolve_ids = resolve_ids, progress = progress)
}
