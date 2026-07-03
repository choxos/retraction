# Reference matching: fuzzy title helpers plus the per-reference orchestration
# that queries the selected sources and reconciles them into one result row.

## ---------------------------------------------------------------------------
## Fuzzy helpers
## ---------------------------------------------------------------------------

#' Default fuzzy title-similarity threshold.
#' @noRd
fuzzy_threshold <- function() getOption("retraction.fuzzy_threshold", 0.90)

#' Jaro-Winkler similarity between a normalized title and a vector of them.
#' @noRd
title_similarity <- function(a, b) {
  1 - stringdist::stringdist(a, b, method = "jw", p = 0.1)
}

#' Publication year from a date string, or `NA`.
#' @noRd
year_of <- function(x) {
  x <- na_if_empty(x)
  if (is.na(x)) return(NA_integer_)
  y <- suppressWarnings(as.integer(substr(x, 1, 4)))
  if (is.na(y)) NA_integer_ else y
}

#' Absolute difference in year, or `NA`.
#' @noRd
year_delta <- function(y1, y2) {
  y1 <- suppressWarnings(as.integer(y1)); y2 <- suppressWarnings(as.integer(y2))
  if (is.na(y1) || is.na(y2)) return(NA_real_)
  abs(y1 - y2)
}

#' Family-ish name tokens from an author string.
#' @noRd
family_tokens <- function(x) {
  x <- tolower(na_if_empty(x))
  if (is.na(x)) return(character(0))
  x <- gsub("[^a-z; ,]", " ", x)
  toks <- unlist(strsplit(x, "[;,[:space:]]+"))
  unique(toks[nchar(toks) >= 3L])
}

#' Count of shared author tokens between two author strings.
#' @noRd
author_overlap <- function(a, b) {
  length(intersect(family_tokens(a), family_tokens(b)))
}

#' Build a fuzzy Xera hit from the best-matching item, or NULL if below
#' threshold.
#' @noRd
fuzzy_hit_from_items <- function(items, ref) {
  if (!length(items) || !is_nonempty_string(ref$title)) return(NULL)
  rt <- normalize_title(ref$title)
  titles <- vapply(items, function(it) {
    normalize_title(na_if_empty(pluck1(it, "title")) %||% "")
  }, character(1))
  sims <- title_similarity(rt, titles)
  best <- which.max(sims)
  if (!length(best) || is.na(sims[best]) || sims[best] < fuzzy_threshold()) {
    return(NULL)
  }
  it <- items[[best]]
  yd <- year_delta(ref$year, year_of(pluck1(it, "original_paper_date")))
  ao <- author_overlap(ref$author, na_if_empty(pluck1(it, "author")))
  conf <- score_match("title_fuzzy", title_sim = sims[best],
                      year_delta = yd, author_overlap = ao)
  build_xera_hit(it, matched_on = "title", match_type = "title_fuzzy",
                 confidence = conf, evidence = "title_fuzzy")
}

## ---------------------------------------------------------------------------
## Per-reference orchestration
## ---------------------------------------------------------------------------

#' Match a single normalized reference against the selected sources.
#' @param ref A normalized reference (see `as_reference()`).
#' @param sources Character vector of validated source names.
#' @param ctx Context list (offline, snapshot, allow_fuzzy, resolve_ids,
#'   flag_nature).
#' @return A one-row result as a named list.
#' @noRd
match_reference <- function(ref, sources, ctx) {
  doi_from_pmid <- FALSE

  # A PMID with no DOI is resolved to a DOI (via OpenAlex) and then matched
  # through the normal DOI path, since the Xera API cannot be queried by PMID.
  if (!is_nonempty_string(ref$doi) && is_nonempty_string(ref$pmid) &&
      !isTRUE(ctx$offline) && isTRUE(ctx$resolve_ids)) {
    rd <- resolve_pmid_to_doi(ref$pmid)
    if (is_nonempty_string(rd)) {
      ref$doi <- rd
      doi_from_pmid <- TRUE
    }
  }

  hits <- lapply(sources, function(s) {
    b <- get_backend(s)
    if (is.null(b)) return(NULL)
    tryCatch(b$fn(ref, ctx), error = function(e) NULL)
  })

  rec <- reconcile_sources(hits)
  finalize_row(ref, rec, ctx, doi_from_pmid)
}

#' Assemble the final result row from a reconciliation outcome.
#' @noRd
finalize_row <- function(ref, rec, ctx, doi_from_pmid = FALSE) {
  flag_nature <- ctx$flag_nature %||% DEFAULT_FLAG_NATURE
  hit <- rec$hit

  status <- rec$status

  match_type <- hit$match_type %||% NA_character_
  matched_on <- hit$matched_on %||% NA_character_
  confidence <- hit$confidence %||% 0

  # If the DOI was recovered from a PMID, report it as a PMID match and cap
  # confidence at the PMID-exact level.
  if (doi_from_pmid && rec$matched) {
    match_type <- "pmid_exact"
    matched_on <- "pmid"
    confidence <- min(confidence, score_match("pmid_exact"))
  }

  # A fuzzy title match is never asserted as retracted; it is reported as a
  # "possible" match for the user to verify. Hard flags require an exact
  # identifier or exact-metadata match above the confidence threshold.
  is_retracted <- isTRUE(rec$matched) &&
    status_is_flagged(status, flag_nature) &&
    confidence >= min_confidence() &&
    !identical(match_type, "title_fuzzy")

  rdate <- hit$retraction_date %||% as.Date(NA)
  days_since <- if (!is.na(rdate)) as.integer(Sys.Date() - rdate) else NA_integer_

  list(
    id = ref$id %||% (ref$doi %||% ref$pmid %||% na_if_empty(ref$title)),
    input_type = ref$input_type %||% NA_character_,
    query = ref$query %||% NA_character_,
    doi = ref$doi %||% NA_character_,
    pmid = ref$pmid %||% NA_character_,
    matched = isTRUE(rec$matched),
    status = status,
    is_retracted = is_retracted,
    confidence = round(confidence, 3),
    match_type = match_type,
    matched_on = matched_on,
    nature = hit$nature %||% NA_character_,
    record_id = hit$record_id %||% NA_character_,
    matched_title = hit$title %||% NA_character_,
    journal = hit$journal %||% NA_character_,
    retraction_date = rdate,
    days_since_retraction = days_since,
    reason = hit$reason %||% NA_character_,
    sources = if (length(rec$confirming)) paste(rec$confirming, collapse = ", ") else NA_character_,
    disagreement = isTRUE(rec$disagreement),
    source_file = ref$source_file %||% NA_character_,
    location = ref$location %||% NA_character_
  )
}
