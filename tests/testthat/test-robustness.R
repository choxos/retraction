# Regression tests for the second hardening pass.

## Identifier normalization -------------------------------------------------

test_that("normalize_pmid rejects non-PMID input instead of fabricating one", {
  expect_true(is.na(normalize_pmid("PMC12345")))
  expect_true(is.na(normalize_pmid("10.1016/foo")))
  expect_true(is.na(normalize_pmid("order #99")))
  expect_equal(normalize_pmid("PMID: 12345678"), "12345678")
})

test_that("normalize_doi strips leading wrappers (angle brackets)", {
  expect_equal(normalize_doi("<10.1016/test>"), "10.1016/test")
  expect_equal(normalize_doi("(10.1/x)"), "10.1/x")
})

test_that("looks_like_doi requires a non-empty suffix", {
  expect_false(looks_like_doi("10.1234/"))
  expect_true(looks_like_doi("10.1234/x"))
})

## Parsers ------------------------------------------------------------------

test_that("bib_fields does not read a DOI from inside another field's value", {
  tf <- tempfile(fileext = ".bib")
  writeLines(c("@article{k,", "  note = {see, doi = 10.1/wrong},",
               "  doi = {10.1/real},", "  date = {2020-03-01}", "}"), tf)
  r <- parse_bib(tf)
  expect_equal(r[[1]]$doi, "10.1/real")
  expect_equal(r[[1]]$year, 2020L)
})

test_that("parse_bib skips @string/@comment/@preamble", {
  tf <- tempfile(fileext = ".bib")
  writeLines(c("@string{jan = {January}}", "@comment{x}",
               "@article{k2, doi = {10.2/x}}"), tf)
  expect_length(parse_bib(tf), 1)
})

test_that("parse_ris reads lowercase tags", {
  tf <- tempfile(fileext = ".ris")
  writeLines(c("ty  - JOUR", "au  - Smith, J", "do  - 10.5/yz", "er  - "), tf)
  r <- parse_ris(tf)
  expect_equal(r[[1]]$doi, "10.5/yz")
  expect_match(r[[1]]$author, "Smith")
})

test_that("parse_csv reads a table with a doi column", {
  tf <- tempfile(fileext = ".csv")
  writeLines(c("doi,title", "10.9/z,Foo"), tf)
  expect_equal(parse_csv(tf)[[1]]$doi, "10.9/z")
})

test_that("extract_braced returns empty on a missing closing brace", {
  expect_equal(extract_braced("{unterminated"), "")
  expect_equal(extract_braced("{ok}rest"), "ok")
})

test_that("a DOI glued to a preceding letter is not scraped", {
  tf <- tempfile(fileext = ".txt")
  writeLines("isbn10.1234/notadoi and 10.5678/real", tf)
  dois <- vapply(parse_text(tf), function(r) r$doi, character(1))
  expect_true("10.5678/real" %in% dois)
  expect_false("10.1234/notadoi" %in% dois)
})

## Matching + scoring -------------------------------------------------------

test_that("family_tokens keeps two-character surnames", {
  expect_true(all(c("li", "wu") %in% family_tokens("Li, X; Wu, Y")))
  expect_equal(author_overlap("Li, X; Wu, Y", "Li, Z"), 1)
})

test_that("fuzzy score is capped below the default confidence threshold", {
  expect_lt(score_match("title_fuzzy", 1.0, 0, 2), min_confidence())
})

test_that("PMID-resolved fuzzy match is still not asserted as retracted", {
  hit <- build_xera_hit(
    list(retraction_nature = "Retraction", original_paper_doi = "10.1/x",
         record_id = "R", title = "T", retraction_date = "2015-01-01"),
    matched_on = "title", match_type = "title_fuzzy", confidence = 0.89
  )
  rec <- list(matched = TRUE, status = "retracted", confirming = "xera",
              checked = "xera", disagreement = FALSE, hit = hit, errored = FALSE)
  ref <- as_reference(pmid = "12345678")
  row <- finalize_row(ref, rec, list(flag_nature = DEFAULT_FLAG_NATURE),
                      doi_from_pmid = TRUE)
  expect_false(row$is_retracted)
})

## Backends + reconciliation ------------------------------------------------

test_that("openalex_verdict treats a notice work (type=retraction) as not flagged", {
  notice <- openalex_verdict(
    list(is_retracted = TRUE, type = "retraction", display_name = "Retraction of X"),
    "10.1/notice")
  expect_equal(notice$status, "none")
  expect_equal(notice$matched_on, "retraction_doi")
  work <- openalex_verdict(
    list(is_retracted = TRUE, type = "article", display_name = "RETRACTED: X"),
    "10.1/orig")
  expect_equal(work$status, "retracted")
})

test_that("pick_governing treats a dateless reinstatement as governing", {
  items <- list(
    list(retraction_nature = "Retraction", retraction_date = "2010-01-01"),
    list(retraction_nature = "Reinstatement", retraction_date = NA)
  )
  expect_equal(classify_status(pick_governing(items)$retraction_nature), "reinstated")
})

test_that("a not-applicable source does not turn a checked-clean result unchecked", {
  rec <- reconcile_sources(list(
    new_hit("xera", 1L, checked = TRUE),          # ok, no match
    new_hit("crossref", 2L)                        # not applicable (no doi)
  ))
  expect_equal(rec$status, "none")
  expect_false(rec$errored)
})

test_that("a failed source with no match is unchecked and records the failure", {
  rec <- reconcile_sources(list(new_hit("xera", 1L, state = "failed")))
  expect_equal(rec$status, "unchecked")
  expect_true(rec$errored)
})

test_that("reconcile records dissenting sources", {
  f <- new_hit("xera", 1L, checked = TRUE, matched = TRUE, status = "retracted",
               matched_on = "original_doi")
  d <- new_hit("openalex", 3L, checked = TRUE, matched = FALSE)
  rec <- reconcile_sources(list(f, d))
  expect_true(rec$disagreement)
  expect_equal(rec$disagreeing, "openalex")
})

## Result counts ------------------------------------------------------------

local({
  mk <- function(...) {
    base <- list(id = NA, input_type = NA, query = NA, doi = NA, pmid = NA,
                 matched = FALSE, status = "none", is_retracted = FALSE,
                 confidence = 0, match_type = NA, matched_on = NA, nature = NA,
                 record_id = NA, matched_title = NA, journal = NA,
                 retraction_date = as.Date(NA), days_since_retraction = NA_integer_,
                 reason = NA, sources = NA, disagreement = FALSE, disagreeing = NA,
                 checked_at = as.Date(NA), source_file = NA, location = NA)
    utils::modifyList(base, list(...))
  }

  test_that("a matched non-retraction notice is counted as a notice, not clean", {
    res <- new_retraction_result(list(
      mk(status = "other", matched = TRUE),
      mk(status = "none", matched = TRUE, matched_on = "retraction_doi"),
      mk(status = "none", matched = FALSE)
    ))
    cnt <- retraction:::result_counts(res)
    expect_equal(cnt$notice, 2)
    expect_equal(cnt$clean, 1)
  })
})

## Sync helpers -------------------------------------------------------------

test_that("parse_docx extracts a DOI from a Word document", {
  skip_if(Sys.which("zip") == "", "external zip tool not available")
  build <- tempfile("docxbuild")
  dir.create(file.path(build, "word"), recursive = TRUE)
  writeLines(
    paste0("<?xml version='1.0'?><w:document xmlns:w='http://x'><w:body>",
           "<w:p><w:r><w:t>See 10.1234/docxexample here.</w:t></w:r></w:p>",
           "</w:body></w:document>"),
    file.path(build, "word", "document.xml")
  )
  docx <- tempfile(fileext = ".docx")
  withr::with_dir(build, utils::zip(docx, files = "word/document.xml"))
  skip_if(!file.exists(docx), "zip did not produce an archive")
  dois <- vapply(parse_docx(docx), function(r) r$doi, character(1))
  expect_true("10.1234/docxexample" %in% dois)
})

test_that("rbind_union aligns columns across frames", {
  a <- data.frame(x = "1", y = "a", stringsAsFactors = FALSE)
  b <- data.frame(x = "2", z = "b", stringsAsFactors = FALSE)
  out <- rbind_union(list(a, b))
  expect_equal(nrow(out), 2)
  expect_true(all(c("x", "y", "z") %in% names(out)))
  expect_true(is.na(out$z[1]))
})
