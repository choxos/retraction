# Source backends and cross-source reconciliation.
#
# Every backend takes a normalized reference and a context, and returns a single
# "hit": a uniform list describing what (if anything) that source knows about
# the reference. Keeping the schema identical across sources lets the matching
# and reporting layers stay source-agnostic.

## ---------------------------------------------------------------------------
## Uniform hit schema
## ---------------------------------------------------------------------------

#' Construct a uniform backend hit.
#' @noRd
new_hit <- function(source, source_priority, checked = FALSE, matched = FALSE,
                    status = "none", doi = NA_character_, pmid = NA_character_,
                    pmcid = NA_character_, record_id = NA_character_,
                    title = NA_character_, journal = NA_character_,
                    nature = NA_character_, notice_type = NA_character_,
                    status_source = NA_character_,
                    retraction_date = as.Date(NA), reason = NA_character_,
                    matched_on = NA_character_, match_type = NA_character_,
                    confidence = 0, evidence = character(0), raw = NULL) {
  list(
    source = source, source_priority = source_priority, checked = checked,
    matched = matched, status = status, doi = doi, pmid = pmid, pmcid = pmcid,
    record_id = record_id, title = title, journal = journal, nature = nature,
    notice_type = notice_type, status_source = status_source,
    retraction_date = retraction_date, reason = reason, matched_on = matched_on,
    match_type = match_type, confidence = confidence, evidence = evidence,
    raw = raw
  )
}

#' Parse an API date (ISO timestamp or date-only) to a `Date`.
#' @noRd
parse_api_date <- function(x) {
  x <- na_if_empty(x)
  if (is.na(x)) return(as.Date(NA))
  tryCatch(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"),
           error = function(e) as.Date(NA))
}

## ---------------------------------------------------------------------------
## Registry
## ---------------------------------------------------------------------------

.backend_registry <- new.env(parent = emptyenv())

#' Register a source backend.
#' @param name Source name (e.g. "xera").
#' @param fn Backend function `function(ref, ctx)` returning a hit.
#' @param priority Lower numbers win during reconciliation.
#' @noRd
register_backend <- function(name, fn, priority) {
  assign(name, list(fn = fn, priority = as.integer(priority)),
         envir = .backend_registry)
  invisible()
}

#' Names of all registered backends.
#' @return A character vector of source names.
#' @export
#' @examples
#' list_backends()
list_backends <- function() sort(ls(.backend_registry))

#' @noRd
get_backend <- function(name) get0(name, envir = .backend_registry, inherits = FALSE)

#' Register the three built-in backends. Called from `.onLoad()`.
#' @noRd
register_builtin_backends <- function() {
  register_backend("xera", backend_xera, priority = 1L)
  register_backend("crossref", backend_crossref, priority = 2L)
  register_backend("openalex", backend_openalex, priority = 3L)
  invisible()
}

#' Resolve and validate a `sources` argument.
#' @noRd
resolve_sources <- function(sources) {
  sources <- as_chr(sources)
  if (!length(sources)) sources <- "xera"
  if (length(sources) == 1L && sources == "all") return(list_backends())
  known <- list_backends()
  unknown <- setdiff(sources, known)
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unknown source{?s}: {.val {unknown}}.",
      "i" = "Available sources: {.val {known}}, or {.val all}."
    ))
  }
  # Keep the caller's order but drop duplicates.
  unique(sources)
}

## ---------------------------------------------------------------------------
## Xera / Retraction Watch backend
## ---------------------------------------------------------------------------

#' Build a Xera hit from one record item (a named list of scalar fields).
#' @param it One Xera record item.
#' @param matched_on What the reference matched on ("original_doi", etc.).
#' @param match_type "doi_exact" or "title_fuzzy".
#' @param confidence Match confidence; defaults to the score for `match_type`.
#' @param evidence Evidence tags.
#' @param status_override Force a status (e.g. "none" for notice-only matches).
#' @noRd
build_xera_hit <- function(it, matched_on, match_type = "doi_exact",
                           confidence = NULL, evidence = match_type,
                           status_override = NULL) {
  nature <- na_if_empty(pluck1(it, "retraction_nature"))
  status <- status_override %||% classify_status(nature)
  new_hit(
    source = "xera", source_priority = 1L, checked = TRUE, matched = TRUE,
    status = status,
    doi = normalize_doi(pluck1(it, "original_paper_doi") %||% NA_character_),
    record_id = na_if_empty(pluck1(it, "record_id")),
    title = na_if_empty(pluck1(it, "title")),
    journal = na_if_empty(pluck1(it, "journal")),
    nature = nature, notice_type = nature, status_source = "retraction-watch",
    retraction_date = parse_api_date(pluck1(it, "retraction_date")),
    reason = na_if_empty(pluck1(it, "reason")),
    matched_on = matched_on, match_type = match_type,
    confidence = confidence %||% score_match(match_type), evidence = evidence,
    raw = it
  )
}

#' Pick the governing record among several sharing a DOI: the most recent by
#' retraction date (so a later Reinstatement supersedes an earlier Retraction).
#' @noRd
pick_governing <- function(items) {
  if (length(items) == 1L) return(items[[1L]])
  dates <- as.Date(vapply(
    items, function(it) as.character(parse_api_date(pluck1(it, "retraction_date"))),
    character(1)
  ))
  ord <- order(dates, decreasing = TRUE, na.last = TRUE)
  items[[ord[1L]]]
}

#' Turn Xera search items into a hit, matching a normalized DOI against both the
#' original-paper DOI (flag) and the retraction-notice DOI (informational).
#' @noRd
xera_items_to_hit <- function(items, ref_doi) {
  if (!length(items)) return(NULL)
  on_original <- Filter(function(it) {
    identical(normalize_doi(pluck1(it, "original_paper_doi") %||% NA_character_), ref_doi)
  }, items)
  if (length(on_original)) {
    return(build_xera_hit(pick_governing(on_original), matched_on = "original_doi"))
  }
  on_notice <- Filter(function(it) {
    identical(normalize_doi(pluck1(it, "retraction_doi") %||% NA_character_), ref_doi)
  }, items)
  if (length(on_notice)) {
    hit <- build_xera_hit(on_notice[[1L]], matched_on = "retraction_doi",
                          status_override = "none")
    hit$evidence <- "cited_retraction_notice"
    return(hit)
  }
  NULL
}

#' Enrich a matched Xera hit with detail-only fields (reason, PMID).
#'
#' The search endpoint omits `reason`, so for a matched record we fetch the
#' detail record once to populate it. Controlled by
#' `getOption("retraction.enrich", TRUE)`.
#' @noRd
xera_enrich_hit <- function(hit) {
  if (!isTRUE(getOption("retraction.enrich", TRUE))) return(hit)
  if (!is_nonempty_string(hit$record_id) || !is.na(hit$reason)) return(hit)
  detail <- xera_paper(hit$record_id)
  if (is.null(detail)) return(hit)
  hit$reason <- na_if_empty(pluck1(detail, "reason"))
  hit$pmid <- na_if_empty(pluck1(detail, "original_paper_pubmed_id"))
  hit
}

#' Detect a citation of the retraction *notice* (not the retracted work).
#'
#' Search items omit `retraction_doi`, so when nothing matched the original DOI
#' we check a few candidates' detail records for a matching notice DOI.
#' @noRd
xera_notice_hit <- function(items, ref_doi, max_check = 5L) {
  if (!isTRUE(getOption("retraction.enrich", TRUE))) return(NULL)
  for (it in utils::head(items, max_check)) {
    rid <- na_if_empty(pluck1(it, "record_id"))
    if (is.na(rid)) next
    detail <- xera_paper(rid)
    if (is.null(detail)) next
    if (identical(normalize_doi(pluck1(detail, "retraction_doi") %||% NA_character_), ref_doi)) {
      hit <- build_xera_hit(detail, matched_on = "retraction_doi",
                            status_override = "none")
      hit$reason <- na_if_empty(pluck1(detail, "reason"))
      hit$evidence <- "cited_retraction_notice"
      return(hit)
    }
  }
  NULL
}

#' Xera backend.
#' @noRd
backend_xera <- function(ref, ctx) {
  unmatched <- new_hit("xera", 1L)

  if (isTRUE(ctx$offline)) {
    snap <- ctx$snapshot
    if (is.null(snap)) return(unmatched)
    return(snapshot_hit(snap, ref, ctx) %||% new_hit("xera", 1L, checked = TRUE))
  }

  # Online: exact DOI first.
  if (is_nonempty_string(ref$doi)) {
    items <- xera_search_doi(ref$doi)
    hit <- xera_items_to_hit(items, ref$doi)
    if (is.null(hit) && length(items)) hit <- xera_notice_hit(items, ref$doi)
    if (!is.null(hit)) return(xera_enrich_hit(hit))
    return(new_hit("xera", 1L, checked = TRUE))
  }

  # Online fuzzy title (best-effort; the API only does substring search).
  if (isTRUE(ctx$allow_fuzzy) && is_nonempty_string(ref$title)) {
    res <- xera_get("search/advanced", list(q = ref$title, per_page = 50L))
    items <- pluck1(res, "items") %||% list()
    hit <- fuzzy_hit_from_items(items, ref)
    if (!is.null(hit)) return(hit)
    return(new_hit("xera", 1L, checked = TRUE))
  }

  unmatched
}

## ---------------------------------------------------------------------------
## Crossref backend (DOI only)
## ---------------------------------------------------------------------------

#' Interpret a Crossref `message` into a hit (pure; unit-testable).
#' @noRd
crossref_verdict <- function(msg, doi) {
  title <- na_if_empty(as_chr(pluck1(msg, "title"))[1])
  evidence <- character(0)
  retracted <- FALSE

  if (!is.na(title) && grepl("^\\s*retracted", title, ignore.case = TRUE)) {
    retracted <- TRUE; evidence <- c(evidence, "title_prefix")
  }
  if (!is.null(pluck1(msg, "relation", "is-retracted-by"))) {
    retracted <- TRUE; evidence <- c(evidence, "relation")
  }
  updates <- pluck1(msg, "update-to")
  if (!is.null(updates)) {
    types <- tolower(as_chr(lapply(updates, function(u) pluck1(u, "type"))))
    if (any(grepl("retract|withdraw", types))) {
      retracted <- TRUE; evidence <- c(evidence, "update_to")
    }
  }

  if (!retracted) return(new_hit("crossref", 2L, checked = TRUE, title = title))
  new_hit(
    "crossref", 2L, checked = TRUE, matched = TRUE, status = "retracted",
    doi = doi, title = title, nature = "Retraction",
    notice_type = "Retraction", status_source = "crossref",
    matched_on = "doi", match_type = "doi_exact",
    confidence = score_match("doi_exact"), evidence = evidence, raw = msg
  )
}

#' @noRd
backend_crossref <- function(ref, ctx) {
  if (isTRUE(ctx$offline) || !is_nonempty_string(ref$doi)) return(new_hit("crossref", 2L))
  msg <- crossref_work(ref$doi)
  if (is.null(msg)) return(new_hit("crossref", 2L))
  crossref_verdict(msg, ref$doi)
}

## ---------------------------------------------------------------------------
## OpenAlex backend (DOI only; derived from Retraction Watch)
## ---------------------------------------------------------------------------

#' Interpret an OpenAlex work into a hit (pure; unit-testable).
#' @noRd
openalex_verdict <- function(work, doi) {
  title <- na_if_empty(pluck1(work, "display_name"))
  if (!isTRUE(pluck1(work, "is_retracted"))) {
    return(new_hit("openalex", 3L, checked = TRUE, title = title))
  }
  new_hit(
    "openalex", 3L, checked = TRUE, matched = TRUE, status = "retracted",
    doi = doi, title = title, nature = "Retraction",
    notice_type = "Retraction", status_source = "openalex",
    matched_on = "doi", match_type = "doi_exact",
    confidence = score_match("doi_exact"), evidence = "is_retracted", raw = work
  )
}

#' @noRd
backend_openalex <- function(ref, ctx) {
  if (isTRUE(ctx$offline) || !is_nonempty_string(ref$doi)) return(new_hit("openalex", 3L))
  work <- openalex_by_doi(ref$doi)
  if (is.null(work)) return(new_hit("openalex", 3L))
  openalex_verdict(work, ref$doi)
}

## ---------------------------------------------------------------------------
## Cross-source reconciliation
## ---------------------------------------------------------------------------

#' Merge per-backend hits for one reference into a single verdict.
#'
#' The highest-priority matched hit sets the headline status and metadata;
#' every confirming source is recorded, and a disagreement flag is set when some
#' consulted sources found a retraction and others did not.
#' @noRd
reconcile_sources <- function(hits) {
  hits <- compact(hits)
  matched <- Filter(function(h) isTRUE(h$matched), hits)
  checked_sources <- vapply(Filter(function(h) isTRUE(h$checked), hits),
                            function(h) h$source, character(1))

  if (!length(matched)) {
    return(list(
      matched = FALSE, status = "none",
      confirming = character(0), checked = checked_sources, disagreement = FALSE,
      hit = NULL
    ))
  }

  ord <- order(vapply(matched, function(h) h$source_priority, numeric(1)))
  best <- matched[[ord[1L]]]
  confirming <- vapply(matched, function(h) h$source, character(1))
  # Disagreement: a flagged verdict, but at least one consulted source did not
  # confirm it.
  disagreement <- status_is_flagged(best$status) &&
    length(setdiff(checked_sources, confirming)) > 0

  list(
    matched = TRUE, status = best$status,
    confirming = unique(confirming), checked = unique(checked_sources),
    disagreement = disagreement, hit = best
  )
}
