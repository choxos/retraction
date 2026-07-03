# Retrieve open-access full-text JATS XML from PubMed Central and check the
# article's reference list for retractions. The caller may supply any of a PMID,
# PMCID, DOI, title, or a whole reference string; each is resolved to a PMC
# article, its open-access status is reported, and if it is available its
# reference list is assessed.
#
# The fetch approach mirrors rtransparency's rt_fetch: NCBI E-utilities EFetch
# (db = pmc) first, with the PMC OAI-PMH service as a fallback. An NCBI API key
# (option `retraction.entrez_key` or the ENTREZ_KEY environment variable) raises
# the rate limit from 3 to 10 requests per second.

NCBI_EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

#' Normalize a PubMed Central identifier
#'
#' Canonicalizes a PMCID to the `PMC#######` form, accepting `"PMC123"`,
#' `"123"`, or `123`.
#'
#' @param x A character or numeric vector of PMCIDs.
#' @return A character vector of `PMC`-prefixed identifiers, `NA` where none is
#'   present.
#' @examples
#' normalize_pmcid(c("PMC5334499", "5334499", "pmc 5334499"))
#' @export
normalize_pmcid <- function(x) {
  p <- sub("(?i)^\\s*pmc", "", trimws(as.character(x)), perl = TRUE)
  p <- gsub("[^0-9]", "", p)
  ifelse(is.na(p) | !nzchar(p), NA_character_, paste0("PMC", p))
}

#' @noRd
pmc_entrez_key <- function() {
  k <- getOption("retraction.entrez_key")
  if (!is_nonempty_string(k)) k <- Sys.getenv("ENTREZ_KEY", "")
  if (nzchar(k)) k else NULL
}

#' Throttle for NCBI: 10/s with an API key, otherwise 3/s.
#' @noRd
ncbi_throttle <- function() {
  if (!is.null(pmc_entrez_key())) list(capacity = 10, fill_time_s = 1)
  else list(capacity = 3, fill_time_s = 1)
}

## ---------------------------------------------------------------------------
## Identifier resolution: PMID / DOI / PMCID / title -> a PMC article
## ---------------------------------------------------------------------------

#' Convert an identifier (PMID, DOI, or PMCID) via the NCBI ID Converter.
#' @return A list with `pmcid`, `pmid`, `doi` (each possibly NA).
#' @noRd
pmc_idconv <- function(id) {
  out <- list(pmcid = NA_character_, pmid = NA_character_, doi = NA_character_)
  res <- http_get_json(
    "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/",
    query = compact(list(ids = id, format = "json", tool = "retraction",
                         email = retraction_mailto()))
  )
  rec <- pluck1(res, "records")
  if (is.null(rec) || !length(rec)) return(out)
  r1 <- rec[[1L]]
  out$pmcid <- normalize_pmcid(na_if_empty(pluck1(r1, "pmcid")))
  out$pmid <- na_if_empty(pluck1(r1, "pmid"))
  out$doi <- normalize_doi(na_if_empty(pluck1(r1, "doi")))
  out
}

#' Find a PMC article id from a title or free-text reference via esearch.
#' @return A PMCID or NA.
#' @noRd
pmc_esearch_title <- function(text) {
  req <- httr2::request(paste0(NCBI_EUTILS, "/esearch.fcgi"))
  q <- compact(list(db = "pmc", term = text, retmax = 1, retmode = "json",
                    api_key = pmc_entrez_key(), email = retraction_mailto(),
                    tool = "retraction"))
  req <- do.call(httr2::req_url_query, c(list(req), q))
  txt <- .http_perform(req, parse = "text", throttle = ncbi_throttle())
  if (is.null(txt) || !nzchar(txt)) return(NA_character_)
  res <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                  error = function(e) NULL)
  uid <- pluck1(res, "esearchresult", "idlist")
  uid <- as_chr(uid)
  if (!length(uid)) NA_character_ else paste0("PMC", uid[1L])
}

#' Resolve a single input to a PMC article.
#' @return A list with `input`, `kind`, `pmcid`, `pmid`, `doi`, `title`.
#' @noRd
pmc_resolve <- function(x) {
  x <- na_if_empty(x)
  out <- list(input = x, kind = NA_character_, pmcid = NA_character_,
              pmid = NA_character_, doi = NA_character_, title = NA_character_)
  if (is.na(x)) return(out)

  if (grepl("(?i)pmc[[:space:]]*[0-9]", x)) {
    out$kind <- "pmcid"; out$pmcid <- normalize_pmcid(x)
  } else if (looks_like_doi(x)) {
    out$kind <- "doi"; out$doi <- normalize_doi(x)[1]
    rec <- pmc_idconv(out$doi); out$pmcid <- rec$pmcid; out$pmid <- rec$pmid
  } else if (looks_like_pmid(x)) {
    out$kind <- "pmid"; out$pmid <- normalize_pmid(x)[1]
    rec <- pmc_idconv(out$pmid); out$pmcid <- rec$pmcid
    out$doi <- rec$doi
  } else {
    out$kind <- "title"; out$title <- x
    out$pmcid <- pmc_esearch_title(x)
  }
  out
}

## ---------------------------------------------------------------------------
## Fetch full-text XML
## ---------------------------------------------------------------------------

#' Fetch PMC JATS XML via EFetch (db = pmc). Returns an xml_document or NULL.
#' @noRd
pmc_efetch <- function(pmcid) {
  num <- sub("^PMC", "", pmcid)
  req <- httr2::request(paste0(NCBI_EUTILS, "/efetch.fcgi"))
  q <- compact(list(db = "pmc", id = num, rettype = "xml",
                    api_key = pmc_entrez_key(), email = retraction_mailto(),
                    tool = "retraction"))
  req <- do.call(httr2::req_url_query, c(list(req), q))
  txt <- .http_perform(req, parse = "text", throttle = ncbi_throttle())
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  tryCatch(xml2::read_xml(txt), error = function(e) NULL)
}

#' Fetch PMC JATS XML via the OAI-PMH service (fallback). Returns a doc or NULL.
#' @noRd
pmc_oai <- function(pmcid) {
  num <- sub("^PMC", "", pmcid)
  req <- httr2::request("https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi")
  q <- list(verb = "GetRecord",
            identifier = paste0("oai:pubmedcentral.nih.gov:", num),
            metadataPrefix = "pmc")
  req <- do.call(httr2::req_url_query, c(list(req), q))
  txt <- .http_perform(req, parse = "text", throttle = ncbi_throttle())
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  doc <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  probe <- tryCatch({
    d <- xml2::read_xml(txt); xml2::xml_ns_strip(d)
    xml2::xml_text(xml2::xml_find_all(d, ".//error"))
  }, error = function(e) character(0))
  if (length(probe) && any(nzchar(probe))) return(NULL)
  doc
}

#' Retrieve open-access PMC full-text XML for a PMCID
#'
#' Tries NCBI EFetch (`db = pmc`) first, then the PMC OAI-PMH service. Results
#' are cached on disk under [retraction_cache_dir()]; an existing non-empty cache
#' file is reused unless `overwrite = TRUE`.
#'
#' @param pmcid A PMCID (any form accepted by [normalize_pmcid()]).
#' @param cache Cache the XML on disk and reuse it on later calls.
#' @param overwrite Re-fetch even if a cache file exists.
#' @return An `xml_document`, or `NULL` if the article could not be retrieved.
#' @examples
#' \dontrun{
#' doc <- pmc_fetch_xml("PMC5334499")
#' }
#' @export
pmc_fetch_xml <- function(pmcid, cache = TRUE, overwrite = FALSE) {
  pmcid <- normalize_pmcid(pmcid)[1]
  if (is.na(pmcid)) return(NULL)
  dest <- file.path(retraction_cache_dir(), "pmc", paste0(pmcid, ".xml"))

  if (cache && !overwrite && file.exists(dest) && file.info(dest)$size > 0) {
    doc <- tryCatch(xml2::read_xml(dest), error = function(e) NULL)
    if (!is.null(doc)) return(doc)
  }

  doc <- pmc_efetch(pmcid) %||% pmc_oai(pmcid)
  if (is.null(doc)) return(NULL)

  if (cache) {
    dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
    tryCatch(xml2::write_xml(doc, dest), error = function(e) NULL)
  }
  doc
}

#' Extract the article's own DOI from a fetched JATS document.
#' @noRd
jats_article_doi <- function(doc) {
  d <- tryCatch(
    {
      xml2::xml_ns_strip(doc)
      doc
    },
    error = function(e) NULL
  )
  if (is.null(d)) return(NA_character_)
  normalize_doi(xml_text1(d, ".//article-meta//article-id[@pub-id-type='doi']"))
}

## ---------------------------------------------------------------------------
## Public entry point
## ---------------------------------------------------------------------------

#' Check the reference lists of open-access PubMed Central articles
#'
#' For each input, resolves it to a PubMed Central article, reports whether the
#' open-access full text (with a reference list) is available, and if so checks
#' every reference in that article for retractions. Inputs may be a PMID, PMCID,
#' DOI, article title, or a whole reference string, in any mix.
#'
#' Resolving and fetching the article always use the network, even when
#' `offline = TRUE` (which controls only the retraction data source used for
#' matching). Per-article open-access status is available via [pmc_articles()].
#'
#' @param x A character (or numeric) vector of PMIDs, PMCIDs, DOIs, titles, or
#'   reference strings.
#' @param cache Cache fetched XML on disk (see [pmc_fetch_xml()]).
#' @inheritParams check_dois
#' @return A [`retraction_result`][print.retraction_result] tibble of the
#'   references found across the open-access articles (the `source_file` column
#'   records the PMCID). The per-article open-access summary is attached and
#'   retrievable with [pmc_articles()].
#' @seealso [pmc_articles()], [pmc_fetch_xml()]
#' @examples
#' \dontrun{
#' res <- check_pmc(c("PMC5334499", "10.1371/journal.pone.0000217", "29939664"))
#' pmc_articles(res)   # open-access status per input
#' retracted(res)      # any retracted references found
#' }
#' @export
check_pmc <- function(x, sources = getOption("retraction.sources", "xera"),
                      offline = FALSE,
                      flag_nature = c("Retraction", "Expression of Concern"),
                      allow_fuzzy = TRUE, resolve_ids = TRUE, cache = TRUE,
                      progress = TRUE) {
  inputs <- as_chr(x)
  if (!length(inputs)) {
    cli::cli_warn("No inputs were provided.")
    return(structure(new_retraction_result(list()),
                     articles = empty_articles()))
  }

  refs <- list()
  arts <- vector("list", length(inputs))
  for (i in seq_along(inputs)) {
    r <- pmc_resolve(inputs[i])
    resolved <- !is.na(r$pmcid)
    doc <- if (resolved) pmc_fetch_xml(r$pmcid, cache = cache) else NULL
    retrieved <- !is.null(doc)
    refs_i <- if (retrieved) parse_jats_doc(doc, source_file = r$pmcid) else list()
    if (length(refs_i)) refs <- c(refs, refs_i)
    adoi <- if (!is.na(r$doi)) r$doi
            else if (retrieved) jats_article_doi(doc) else NA_character_
    # Open access here means the full-text XML was retrieved. Resolution and
    # retrieval are tracked separately so a resolution or network failure is not
    # reported as "not open access".
    arts[[i]] <- list(
      input = inputs[i], pmcid = r$pmcid %||% NA_character_, doi = adoi,
      resolved = resolved, retrieved = retrieved, is_open_access = retrieved,
      n_references = length(refs_i)
    )
  }

  result <- if (length(refs)) {
    run_checks(refs, sources, offline, flag_nature, allow_fuzzy, resolve_ids, progress)
  } else {
    new_retraction_result(list())
  }

  articles <- articles_tibble(arts, result)
  attr(result, "articles") <- articles
  report_pmc(articles, result)
  result
}

#' Per-article open-access summary from a [check_pmc()] result
#'
#' @param x A `retraction_result` returned by [check_pmc()].
#' @return A tibble with `input`, `pmcid`, `doi`, `resolved` (a PMC article was
#'   found), `retrieved` (its full text was fetched), `is_open_access` (equal to
#'   `retrieved`), `n_references`, and `n_retracted`.
#' @export
pmc_articles <- function(x) {
  a <- attr(x, "articles")
  if (is.null(a)) empty_articles() else a
}

#' @noRd
empty_articles <- function() {
  tibble::tibble(input = character(), pmcid = character(), doi = character(),
                 resolved = logical(), retrieved = logical(),
                 is_open_access = logical(), n_references = integer(),
                 n_retracted = integer())
}

#' @noRd
articles_tibble <- function(arts, result) {
  if (!length(arts)) return(empty_articles())
  chr <- function(nm) vapply(arts, function(a) a[[nm]] %||% NA_character_, character(1))
  lg <- function(nm) vapply(arts, function(a) isTRUE(a[[nm]]), logical(1))
  pmcid <- chr("pmcid")
  n_ret <- vapply(pmcid, function(id) {
    if (is.na(id)) return(0L)
    sum(result$is_retracted %in% TRUE & result$source_file %in% id)
  }, integer(1))
  tibble::tibble(
    input = chr("input"), pmcid = pmcid, doi = chr("doi"),
    resolved = lg("resolved"), retrieved = lg("retrieved"),
    is_open_access = lg("is_open_access"),
    n_references = vapply(arts, function(a) as.integer(a$n_references %||% 0L), integer(1)),
    n_retracted = unname(n_ret)
  )
}

#' @noRd
report_pmc <- function(articles, result) {
  n <- nrow(articles)
  oa <- sum(articles$is_open_access)
  cli::cli_h2("PubMed Central: {n} article{?s} checked")
  cli::cli_alert_info("{oa} open access, {n - oa} not available")
  for (i in seq_len(n)) {
    a <- articles[i, ]
    if (isTRUE(a$is_open_access)) {
      msg <- "{a$pmcid}: open access, {a$n_references} reference{?s}, {a$n_retracted} retracted"
      if (a$n_retracted > 0) cli::cli_alert_danger(msg) else cli::cli_alert_success(msg)
    } else if (isTRUE(a$resolved)) {
      cli::cli_alert_warning(
        "{a$pmcid}: in PubMed Central, but the open-access full text was not retrievable")
    } else {
      cli::cli_alert_warning("{a$input}: could not be resolved to a PubMed Central article")
    }
  }
  invisible()
}
