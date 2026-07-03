# Tests for the v0.1.0 feature set (timing, exposure, watch, review, reasons,
# annotate, export, extras, policy, strong-metadata tier).

row <- function(...) {
  utils::modifyList(list(
    id = NA_character_, input_type = NA_character_, query = NA_character_,
    doi = NA_character_, pmid = NA_character_, matched = FALSE, status = "none",
    is_retracted = FALSE, confidence = 0, match_type = NA_character_,
    matched_on = NA_character_, nature = NA_character_, record_id = NA_character_,
    matched_title = NA_character_, journal = NA_character_,
    retraction_date = as.Date(NA), days_since_retraction = NA_integer_,
    reason = NA_character_, sources = NA_character_, disagreement = FALSE,
    disagreeing = NA_character_, checked_at = as.Date(NA),
    source_file = NA_character_, location = NA_character_
  ), list(...))
}
res <- function(...) new_retraction_result(list(...))

## Strong-metadata tier (B8) -------------------------------------------------

test_that("exact title + year + first author promotes to title_exact", {
  items <- list(list(title = "A distinctive study of ostriches in winter light",
                     original_paper_doi = "10.1/x", original_paper_date = "2010-01-01",
                     author = "Smith, J; Doe, A", retraction_nature = "Retraction",
                     record_id = "R1"))
  ref <- as_reference(title = "A distinctive study of ostriches in winter light",
                      year = 2010, author = "Smith, J; Doe, A")
  hit <- fuzzy_hit_from_items(items, ref)
  expect_equal(hit$match_type, "title_exact")
  expect_gte(hit$confidence, min_confidence())
})

test_that("a short generic title is not promoted without two shared authors", {
  items <- list(list(title = "Methods", original_paper_doi = "10.1/x",
                     original_paper_date = "2010-01-01", author = "Smith, J",
                     retraction_nature = "Retraction", record_id = "R1"))
  ref <- as_reference(title = "Methods", year = 2010, author = "Smith, J")
  hit <- fuzzy_hit_from_items(items, ref)
  expect_equal(hit$match_type, "title_fuzzy")
  expect_lt(hit$confidence, min_confidence())
})

## Temporal ------------------------------------------------------------------

test_that("classify_timing defaults to conservative document labels", {
  x <- res(row(matched = TRUE, is_retracted = TRUE,
               retraction_date = as.Date("2010-01-01")))
  expect_equal(classify_timing(x, "2015-01-01")$timing, "document_after_retraction")
  expect_equal(classify_timing(x, "2005-01-01")$timing, "document_before_retraction")
})

test_that("citation_dates unlock cited_ labels", {
  x <- res(row(id = "k", matched = TRUE, is_retracted = TRUE,
               retraction_date = as.Date("2010-01-01")))
  out <- classify_timing(x, "2015-01-01", citation_dates = c(k = "2008-01-01"))
  expect_equal(out$timing, "cited_before_retraction")
})

test_that("exposure_score carries denominator diagnostics", {
  x <- res(row(matched = TRUE, is_retracted = TRUE, status = "retracted"),
           row(status = "unchecked"), row(status = "none"))
  e <- exposure_score(x)
  expect_equal(e$n_total, 3L)
  expect_equal(e$n_flagged, 1L)
  expect_equal(e$n_unchecked, 1L)
  expect_equal(e$n_checked, 2L)
  expect_equal(e$flagged_per_checked, 0.5)
})

## Reasons -------------------------------------------------------------------

test_that("reason bucketing is primary + multi-label", {
  expect_equal(primary_reason_bucket("Fabrication of data"), "misconduct")
  expect_equal(primary_reason_bucket("Nothing recognizable"), "other")
  expect_true(all(c("misconduct", "process") %in%
                    reason_buckets("Data fabrication and authorship dispute")))
})

## Policy + plumbing ---------------------------------------------------------

test_that("fail_policy triggers on selected states only", {
  x <- res(row(matched = TRUE, is_retracted = TRUE, status = "retracted"),
           row(status = "unchecked"))
  expect_true(evaluate_policy(x, fail_policy("flagged"))$fail)
  expect_false(evaluate_policy(x, fail_policy("possible"))$fail)
  expect_true(evaluate_policy(x, fail_policy("unchecked"))$fail)
})

test_that("bind_results preserves class and Date columns", {
  x <- res(row(retraction_date = as.Date("2010-01-01")))
  out <- bind_results(list(x, x))
  expect_s3_class(out, "retraction_result")
  expect_s3_class(out$retraction_date, "Date")
  expect_equal(nrow(out), 2L)
})

test_that("retraction_scan fails closed on a missing file", {
  expect_error(retraction_scan("/no/such/path/refs.bib"), "not found")
})

## Extras --------------------------------------------------------------------

test_that("explain_result and compare_sources read the verdict columns", {
  x <- res(
    row(id = "a", matched = TRUE, is_retracted = TRUE, status = "retracted",
        matched_on = "original_doi", sources = "xera", confidence = 1),
    row(id = "b", matched = TRUE, status = "none", matched_on = "retraction_doi",
        sources = "xera", disagreement = TRUE, disagreeing = "openalex", confidence = 1)
  )
  ex <- explain_result(x)
  expect_equal(nrow(ex), 2L)
  expect_match(ex$explanation[1], "flagged")
  cs <- compare_sources(x)
  expect_equal(nrow(cs), 1L)
  expect_equal(cs$dissenting, "openalex")
})

test_that("badge_json writes a shields endpoint", {
  x <- res(row(is_retracted = TRUE), row(is_retracted = FALSE))
  p <- tempfile(fileext = ".json")
  badge_json(x, p)
  j <- jsonlite::fromJSON(p)
  expect_equal(j$message, "1")
  expect_equal(j$color, "red")
})

test_that("export_result writes csv and json", {
  x <- res(row(id = "a", doi = "10.1/x", is_retracted = TRUE))
  csv <- tempfile(fileext = ".csv"); export_result(x, csv)
  expect_true(any(grepl("10.1/x", readLines(csv))))
  json <- tempfile(fileext = ".json"); export_result(x, json)
  expect_true(file.exists(json))
  expect_error(export_result(x, tempfile(fileext = ".parquet")), "Unsupported")
})

## Annotate ------------------------------------------------------------------

test_that("annotate_bib marks the flagged entry and is idempotent", {
  bib <- tempfile(fileext = ".bib")
  writeLines(c("@article{key1,", "  title = {A paper},", "  doi = {10.1/x}", "}",
               "@article{key2,", "  title = {Clean paper}", "}"), bib)
  x <- res(row(id = "key1", doi = "10.1/x", matched = TRUE, is_retracted = TRUE,
               status = "retracted", reason = "Fabrication"))
  out <- tempfile(fileext = ".bib")
  annotate_bib(bib, x, out)
  n1 <- sum(grepl("RETRACTED", readLines(out)))
  annotate_bib(out, x, out)
  n2 <- sum(grepl("RETRACTED", readLines(out)))
  expect_equal(n1, 1L)
  expect_equal(n2, 1L)
})

## Watch (hermetic cache) ----------------------------------------------------

test_that("watch diff detects newly-flagged references by stable identifier", {
  withr::local_envvar(R_USER_CACHE_DIR = tempfile("rcache"))
  base <- res(row(id = "a", doi = "10.1/x", is_retracted = FALSE, status = "none"))
  retraction_watch_save(base, "t1")
  now <- res(row(id = "a", doi = "10.1/x", is_retracted = TRUE, status = "retracted"))
  expect_equal(nrow(retraction_watch_diff(now, "t1")), 1L)
  # Same DOI under a different display id still matches (key stability).
  reordered <- res(row(id = "zzz", doi = "10.1/x", is_retracted = TRUE, status = "retracted"))
  expect_equal(nrow(retraction_watch_diff(reordered, "t1")), 1L)
})

test_that("watch refuses a reserved name", {
  expect_error(retraction_watch_save(res(row()), "snapshot"), "reserved")
})

## Native PMID matching (B7) -------------------------------------------------

test_that("offline snapshot matches by PubMed ID", {
  snap <- data.frame(
    record_id = "R1", title = "A retracted paper", original_paper_doi = "10.1/x",
    retraction_doi = "10.1/x.notice", original_paper_pubmed_id = "111",
    retraction_pubmed_id = "222", journal = "J", author = "Smith, J",
    original_paper_date = "2010-01-01", retraction_date = "2012-01-01",
    retraction_nature = "Retraction", reason = "Fabrication",
    stringsAsFactors = FALSE
  )
  snap <- add_norm_columns(snap)
  ctx <- list(offline = TRUE, snapshot = snap, allow_fuzzy = FALSE)
  h1 <- snapshot_hit(snap, as_reference(pmid = "111"), ctx)
  expect_equal(h1$status, "retracted")
  expect_equal(h1$matched_on, "pmid")
  expect_equal(h1$match_type, "pmid_exact")
  # Citing the retraction notice's PMID is matched but not flagged.
  h2 <- snapshot_hit(snap, as_reference(pmid = "222"), ctx)
  expect_equal(h2$status, "none")
  # An unknown PMID does not match.
  expect_null(snapshot_hit(snap, as_reference(pmid = "999"), ctx))
})

test_that("a snapshot without PMID columns still works (older export)", {
  snap <- data.frame(
    record_id = "R1", title = "A paper", original_paper_doi = "10.1/x",
    retraction_date = "2012-01-01", retraction_nature = "Retraction",
    stringsAsFactors = FALSE
  )
  snap <- add_norm_columns(snap)
  ctx <- list(offline = TRUE, snapshot = snap, allow_fuzzy = FALSE)
  # No PMID columns -> all-NA norm columns -> PMID lookup no-ops, no error.
  expect_null(snapshot_hit(snap, as_reference(pmid = "111"), ctx))
})

## Offline DOI index (F28) ---------------------------------------------------

test_that("the offline DOI index matches and fast-rejects", {
  snap <- data.frame(
    record_id = "R1", title = "P", original_paper_doi = "10.1/x",
    retraction_date = "2012-01-01", retraction_nature = "Retraction",
    stringsAsFactors = FALSE
  )
  snap <- attach_doi_index(add_norm_columns(snap))
  expect_true(is.environment(attr(snap, "doi_index")))
  ctx <- list(offline = TRUE, snapshot = snap, allow_fuzzy = FALSE)
  expect_equal(snapshot_hit(snap, as_reference(doi = "10.1/x"), ctx)$status, "retracted")
  expect_null(snapshot_hit(snap, as_reference(doi = "10.9/z"), ctx))
})

## Audit-3 regression tests -------------------------------------------------

test_that("a throwing backend is recorded as a failure and strict errors cleanly", {
  register_backend("boomtest", function(ref, ctx) stop("boom"), priority = 1L)
  withr::defer(rm(list = "boomtest", envir = .backend_registry))
  res <- check_dois("10.1/x", sources = "boomtest", progress = FALSE)
  expect_equal(res$status, "unchecked")           # failure represented, not clean
  err <- tryCatch(
    check_dois("10.1/x", sources = "boomtest", strict = TRUE, progress = FALSE),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "could not be checked")
  expect_false(grepl("pluralize", err))            # cli quantity fix
})

test_that("check_file strict errors on a missing file and an empty parse", {
  expect_error(check_file("definitely-missing.bib", strict = TRUE, progress = FALSE),
               "not found")
  empty <- tempfile(fileext = ".txt"); writeLines("no identifiers here", empty)
  expect_error(check_file(empty, strict = TRUE, progress = FALSE), "No references")
})

test_that("crossref_verdict ignores an update-to entry that has no type", {
  v <- crossref_verdict(list(title = list("Plain work"),
                             `update-to` = list(list(DOI = "10.1/orig"))), "10.1/n")
  expect_false(v$matched)
})

test_that("datacite_verdict respects relation direction", {
  passive <- list(data = list(attributes = list(titles = list(list(title = "D")),
    relatedIdentifiers = list(list(relationType = "IsObsoletedBy")))))
  expect_equal(datacite_verdict(passive, "10.5/d")$status, "retracted")
  active <- list(data = list(attributes = list(titles = list(list(title = "D")),
    relatedIdentifiers = list(list(relationType = "Obsoletes")))))
  expect_false(datacite_verdict(active, "10.5/d")$matched)
})

test_that("explain_result accepts integer row indices", {
  x <- res(row(id = "a", matched = TRUE, is_retracted = TRUE, status = "retracted",
               matched_on = "original_doi", confidence = 1), row(id = "b"))
  ex <- explain_result(x, rows = 1L)
  expect_equal(nrow(ex), 1L)
  expect_equal(ex$id, "a")
})

test_that("title_exact matching is robust to author name order", {
  items <- list(list(title = "A distinctive study of ostriches in winter light",
                     original_paper_doi = "10.1/x", original_paper_date = "2010-01-01",
                     author = "Smith, John", retraction_nature = "Retraction",
                     record_id = "R1"))
  ref <- as_reference(title = "A distinctive study of ostriches in winter light",
                      year = 2010, author = "John Smith")
  expect_equal(fuzzy_hit_from_items(items, ref)$match_type, "title_exact")
})

test_that("check_included_studies dedupes and reports counts", {
  register_backend("faketest", function(ref, ctx) {
    if (identical(ref$doi, "10.1234/retracted")) {
      build_xera_hit(list(retraction_nature = "Retraction",
                          original_paper_doi = "10.1234/retracted", record_id = "R",
                          title = "T", retraction_date = "2010-01-01"),
                     matched_on = "original_doi")
    } else {
      new_hit("faketest", 1L, checked = TRUE)
    }
  }, priority = 1L)
  withr::defer(rm(list = "faketest", envir = .backend_registry))
  r <- check_included_studies(c("10.1234/retracted", "10.1234/clean", "10.1234/clean"),
                              sources = "faketest", resolve_ids = FALSE, progress = FALSE)
  expect_s3_class(r, "retraction_review")
  expect_equal(attr(r, "n_included"), 3L)
  expect_equal(attr(r, "n_unique"), 2L)
  expect_equal(attr(r, "n_retracted"), 1L)
})

test_that("retraction_knit_check fails closed on a missing bibliography", {
  expect_error(retraction_knit_check(bib = "definitely-missing.bib", action = "error"),
               "cannot read")
})

test_that("manuscript_date_of falls back to file mtime", {
  f <- tempfile(); writeLines("x", f)
  expect_s3_class(manuscript_date_of(f), "Date")
})

test_that("annotate_bib is idempotent even with a @string block present", {
  bib <- tempfile(fileext = ".bib")
  writeLines(c("@article{key1,", "  title = {A paper},", "  doi = {10.1234/x}", "}",
               "@string{jan = {January}}",
               "@article{key2,", "  title = {Clean}", "}"), bib)
  x <- res(row(id = "key1", doi = "10.1234/x", matched = TRUE, is_retracted = TRUE,
               status = "retracted", reason = "Fabrication"))
  out <- tempfile(fileext = ".bib")
  annotate_bib(bib, x, out)
  annotate_bib(out, x, out)
  expect_equal(sum(grepl("RETRACTED", readLines(out))), 1L)
})

test_that("watch_key is stable for a title-only reference across formatting", {
  a <- res(row(id = "The  Study Of Things", doi = NA_character_, pmid = NA_character_))
  b <- res(row(id = "the study of things", doi = NA_character_, pmid = NA_character_))
  expect_equal(watch_key(a), watch_key(b))
})
