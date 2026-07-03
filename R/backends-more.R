# Additional retraction sources: Europe PMC, NCBI PubMed (eutils), and DataCite.
# Each returns the uniform hit schema with explicit failed/not_applicable state,
# and is registered in register_builtin_backends(). They are opt-in (selected via
# `sources = ...` or `"all"`); the default remains "xera".

## ---------------------------------------------------------------------------
## Europe PMC (publication type + comment/correction list)
## ---------------------------------------------------------------------------

#' @noRd
europepmc_search <- function(doi) {
  res <- http_get_json(
    "https://www.ebi.ac.uk/europepmc/webservices/rest/search",
    list(query = sprintf('DOI:"%s"', doi), format = "json",
         resultType = "core", pageSize = 5)
  )
  if (is.null(res)) return(NULL)
  pluck1(res, "resultList", "result") %||% list()
}

#' Interpret a Europe PMC record into a hit (pure; unit-testable).
#' @noRd
europepmc_verdict <- function(results, doi) {
  if (!length(results)) return(new_hit("europepmc", 4L, checked = TRUE))
  r <- results[[1L]]
  title <- na_if_empty(pluck1(r, "title"))
  pubtypes <- tolower(unlist(pluck1(r, "pubTypeList", "pubType") %||% list()))
  ccl <- pluck1(r, "commentCorrectionList", "commentCorrection") %||% list()
  ccl_types <- tolower(vapply(ccl, function(c) na_if_empty(pluck1(c, "type")) %||% "",
                              character(1)))

  notice <- function(label, ev) {
    new_hit("europepmc", 4L, checked = TRUE, matched = TRUE, status = "none",
            doi = doi, title = title, notice_type = label,
            status_source = "europe-pmc", matched_on = "retraction_doi",
            match_type = "doi_exact", confidence = score_match("doi_exact"),
            evidence = ev, raw = r)
  }
  flagged <- function(status, ev) {
    new_hit("europepmc", 4L, checked = TRUE, matched = TRUE, status = status,
            doi = doi, title = title, nature = status_label(status),
            notice_type = status_label(status), status_source = "europe-pmc",
            matched_on = "doi", match_type = "doi_exact",
            confidence = score_match("doi_exact"), evidence = ev, raw = r)
  }

  # This DOI is itself a notice.
  if ("retraction of publication" %in% pubtypes) return(notice("Retraction", "pubtype_notice"))
  if ("published erratum" %in% pubtypes) return(notice("Correction", "pubtype_erratum"))
  # This DOI is the affected work.
  if ("retracted publication" %in% pubtypes || any(grepl("^retraction in", ccl_types))) {
    return(flagged("retracted", "pubtype"))
  }
  if (any(grepl("expression of concern", ccl_types))) {
    return(flagged("expression_of_concern", "comment_correction"))
  }
  new_hit("europepmc", 4L, checked = TRUE, title = title)
}

#' @noRd
backend_europepmc <- function(ref, ctx) {
  if (isTRUE(ctx$offline) || !is_nonempty_string(ref$doi)) {
    return(new_hit("europepmc", 4L))
  }
  res <- europepmc_search(ref$doi)
  if (is.null(res)) return(new_hit("europepmc", 4L, state = "failed"))
  europepmc_verdict(res, ref$doi)
}

## ---------------------------------------------------------------------------
## NCBI PubMed via E-utilities (publication type)
## ---------------------------------------------------------------------------

#' Polite E-utilities parameters (tool + email when a mailto is configured).
#' @noRd
eutils_params <- function(extra) {
  mailto <- getOption("retraction.mailto", Sys.getenv("RETRACTION_MAILTO", ""))
  c(extra, list(tool = "retraction",
                email = if (nzchar(mailto)) mailto else NULL))
}

#' Resolve a DOI to a PMID via esearch. NULL on failure, NA when none.
#' @noRd
ncbi_pmid_for_doi <- function(doi) {
  res <- http_get_json(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    eutils_params(list(db = "pubmed", term = paste0(doi, "[AID]"),
                       retmode = "json"))
  )
  if (is.null(res)) return(NULL)
  ids <- pluck1(res, "esearchresult", "idlist") %||% list()
  if (!length(ids)) return(NA_character_)
  as.character(ids[[1L]])
}

#' Publication types for a PMID via esummary. NULL on failure.
#' @noRd
ncbi_pubtypes <- function(pmid) {
  res <- http_get_json(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
    eutils_params(list(db = "pubmed", id = pmid, retmode = "json"))
  )
  if (is.null(res)) return(NULL)
  tolower(unlist(pluck1(res, "result", pmid, "pubtype") %||% list()))
}

#' Interpret PubMed publication types into a hit (pure; unit-testable).
#' @noRd
ncbi_verdict <- function(pubtypes, pmid, doi) {
  base <- function(...) new_hit("ncbi", 5L, checked = TRUE, pmid = pmid, doi = doi, ...)
  if ("retraction of publication" %in% pubtypes) {
    return(base(matched = TRUE, status = "none", notice_type = "Retraction",
                status_source = "pubmed", matched_on = "retraction_doi",
                match_type = "pmid_exact", confidence = score_match("pmid_exact"),
                evidence = "pubtype_notice"))
  }
  if ("retracted publication" %in% pubtypes) {
    return(base(matched = TRUE, status = "retracted", nature = "Retraction",
                notice_type = "Retraction", status_source = "pubmed",
                matched_on = "pmid", match_type = "pmid_exact",
                confidence = score_match("pmid_exact"), evidence = "pubtype"))
  }
  base()
}

#' @noRd
backend_ncbi <- function(ref, ctx) {
  if (isTRUE(ctx$offline)) return(new_hit("ncbi", 5L))
  pmid <- ref$pmid
  if (!is_nonempty_string(pmid)) {
    if (!is_nonempty_string(ref$doi)) return(new_hit("ncbi", 5L))
    pmid <- ncbi_pmid_for_doi(ref$doi)
    if (is.null(pmid)) return(new_hit("ncbi", 5L, state = "failed"))
    if (is.na(pmid)) return(new_hit("ncbi", 5L, checked = TRUE))
  }
  pt <- ncbi_pubtypes(pmid)
  if (is.null(pt)) return(new_hit("ncbi", 5L, state = "failed"))
  ncbi_verdict(pt, pmid, ref$doi %||% NA_character_)
}

## ---------------------------------------------------------------------------
## DataCite (retracted datasets, via relatedIdentifiers)
## ---------------------------------------------------------------------------

#' Interpret a DataCite record into a hit (pure; unit-testable).
#' @noRd
datacite_verdict <- function(res, doi) {
  attrs <- pluck1(res, "data", "attributes")
  if (is.null(attrs)) return(new_hit("datacite", 6L))
  title <- na_if_empty(pluck1(attrs, "titles", 1, "title"))
  rels <- pluck1(attrs, "relatedIdentifiers") %||% list()
  rtypes <- tolower(vapply(rels, function(x) na_if_empty(pluck1(x, "relationType")) %||% "",
                           character(1)))
  # DataCite has no dedicated retraction flag; a retraction/obsoletion relation
  # is the best available signal for a withdrawn dataset.
  if (any(grepl("retract|obsolet|withdraw", rtypes))) {
    return(new_hit("datacite", 6L, checked = TRUE, matched = TRUE, status = "retracted",
                   doi = doi, title = title, nature = "Retraction",
                   notice_type = "Retraction", status_source = "datacite",
                   matched_on = "doi", match_type = "doi_exact",
                   confidence = score_match("doi_exact"), evidence = "related_identifier",
                   raw = res))
  }
  new_hit("datacite", 6L, checked = TRUE, title = title)
}

#' @noRd
backend_datacite <- function(ref, ctx) {
  if (isTRUE(ctx$offline) || !is_nonempty_string(ref$doi)) return(new_hit("datacite", 6L))
  res <- http_get_json(paste0("https://api.datacite.org/dois/",
                              utils::URLencode(ref$doi)))
  # A non-DataCite DOI 404s; not distinguishable from a network error here, so it
  # is treated as not applicable (this source is supplementary, dataset-focused).
  if (is.null(res)) return(new_hit("datacite", 6L))
  datacite_verdict(res, ref$doi)
}
