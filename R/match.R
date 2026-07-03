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
  # Keep tokens of length >= 2 so short surnames (Li, Wu, Kim, An) are retained;
  # single-letter initials are still dropped as noise.
  unique(toks[nchar(toks) >= 2L])
}

#' Count of shared author tokens between two author strings.
#' @noRd
author_overlap <- function(a, b) {
  length(intersect(family_tokens(a), family_tokens(b)))
}

#' Family name of the first author, order-robust.
#'
#' Handles both "Family, Given; ..." and "Given Family, ..." formats: a comma in
#' the first author means the family name precedes it, otherwise the last token
#' is taken as the family name.
#' @noRd
first_family <- function(x) {
  x <- na_if_empty(x)
  if (is.na(x)) return(NA_character_)
  first <- trimws(strsplit(x, ";", fixed = TRUE)[[1L]][1L])
  fam <- if (grepl(",", first, fixed = TRUE)) {
    sub(",.*", "", first)
  } else {
    toks <- strsplit(trimws(gsub("[^[:alpha:] ]", " ", first)), "\\s+")[[1L]]
    if (length(toks)) toks[length(toks)] else ""
  }
  na_if_empty(tolower(trimws(fam)))
}

#' Do two author strings share the same first-author family name?
#' @noRd
first_author_matches <- function(a, b) {
  fa <- first_family(a); fb <- first_family(b)
  !is.na(fa) && !is.na(fb) && identical(fa, fb)
}

#' Is a no-identifier title match strong enough to assert (title_exact)?
#'
#' Requires a near-perfect title, the exact year, and a shared first author.
#' Short/generic titles are the main false-positive risk, so a title with fewer
#' than five distinctive tokens additionally requires two shared author tokens.
#' @noRd
is_exact_metadata <- function(title_sim, ref, it) {
  auth <- na_if_empty(pluck1(it, "author"))
  n_tokens <- length(strsplit(normalize_title(ref$title %||% ""), " ")[[1L]])
  min_authors <- if (n_tokens < 5L) 2L else 1L
  isTRUE(title_sim >= 0.985) &&
    year_delta(ref$year, year_of(pluck1(it, "original_paper_date"))) %in% 0 &&
    author_overlap(ref$author, auth) >= min_authors &&
    first_author_matches(ref$author, auth)
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
  mt <- if (is_exact_metadata(sims[best], ref, it)) "title_exact" else "title_fuzzy"
  conf <- score_match(mt, title_sim = sims[best], year_delta = yd,
                      author_overlap = ao)
  build_xera_hit(it, matched_on = "title", match_type = mt,
                 confidence = conf, evidence = mt)
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

  # A PMID with no DOI is resolved to a DOI and then matched through the normal
  # DOI path. The Xera corpus is queried by PMID first (authoritative, since it
  # is the Retraction Watch data); OpenAlex is only a fallback to obtain a DOI
  # for the other DOI-only sources when Xera has no record for the PMID.
  if (!is_nonempty_string(ref$doi) && is_nonempty_string(ref$pmid) &&
      !isTRUE(ctx$offline)) {
    rd <- xera_pmid_to_doi(ref$pmid)
    if (!is_nonempty_string(rd) && isTRUE(ctx$resolve_ids)) {
      rd <- resolve_pmid_to_doi(ref$pmid)
    }
    if (is_nonempty_string(rd)) {
      ref$doi <- rd
      doi_from_pmid <- TRUE
    }
  }

  hits <- lapply(sources, function(s) {
    b <- get_backend(s)
    if (is.null(b)) return(NULL)
    # A backend that throws becomes a failed hit (not NULL), so the failure is
    # represented in reconciliation and caught by strict mode rather than being
    # silently dropped and reported as clean.
    tryCatch(
      b$fn(ref, ctx),
      error = function(e) {
        new_hit(s, b$priority, state = "failed", evidence = conditionMessage(e))
      }
    )
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

  # Record whether the underlying match was fuzzy BEFORE any relabeling, so the
  # "fuzzy is never asserted" guard cannot be bypassed by the PMID relabel.
  was_fuzzy <- identical(match_type, "title_fuzzy")

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
    !was_fuzzy

  # For an offline snapshot, "checked" and "days since" are relative to the
  # snapshot's fetch date, not today, so a frozen snapshot does not drift.
  checked_at <- if (isTRUE(ctx$offline)) {
    d <- attr(ctx$snapshot, "synced_at")
    if (is.null(d)) Sys.Date() else as.Date(d)
  } else {
    Sys.Date()
  }
  rdate <- hit$retraction_date %||% as.Date(NA)
  days_since <- if (!is.na(rdate)) as.integer(checked_at - rdate) else NA_integer_

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
    disagreeing = if (length(rec$disagreeing)) {
      paste(rec$disagreeing, collapse = ", ")
    } else {
      NA_character_
    },
    checked_at = checked_at,
    source_file = ref$source_file %||% NA_character_,
    location = ref$location %||% NA_character_
  )
}
