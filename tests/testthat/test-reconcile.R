# Regression tests for source-status semantics and cross-source reconciliation.

test_that("a failed source with no match is unchecked, not clean", {
  rec <- reconcile_sources(list(new_hit("xera", 1L, checked = FALSE)))
  expect_false(rec$matched)
  expect_equal(rec$status, "unchecked")
})

test_that("all sources checked with no match is clean", {
  rec <- reconcile_sources(list(
    new_hit("xera", 1L, checked = TRUE),
    new_hit("openalex", 3L, checked = TRUE)
  ))
  expect_equal(rec$status, "none")
})

test_that("a notice-DOI clearing beats a flagging source, with disagreement", {
  n <- new_hit("xera", 1L, checked = TRUE, matched = TRUE, status = "none",
               matched_on = "retraction_doi")
  f <- new_hit("openalex", 3L, checked = TRUE, matched = TRUE, status = "retracted",
               matched_on = "doi")
  rec <- reconcile_sources(list(n, f))
  expect_equal(rec$status, "none")
  expect_true(rec$disagreement)
})

test_that("a reinstatement clears a retraction flag, with disagreement", {
  ri <- new_hit("xera", 1L, checked = TRUE, matched = TRUE, status = "reinstated",
                matched_on = "original_doi")
  f <- new_hit("openalex", 3L, checked = TRUE, matched = TRUE, status = "retracted",
               matched_on = "doi")
  rec <- reconcile_sources(list(ri, f))
  expect_equal(rec$status, "reinstated")
  expect_true(rec$disagreement)
})

test_that("a retraction is not suppressed by a higher-priority correction", {
  correction <- new_hit("hc", 1L, checked = TRUE, matched = TRUE, status = "correction",
                        matched_on = "original_doi")
  retraction <- new_hit("lr", 9L, checked = TRUE, matched = TRUE, status = "retracted",
                        matched_on = "original_doi")
  rec <- reconcile_sources(list(correction, retraction))
  expect_equal(rec$status, "retracted")
  expect_true(rec$disagreement)
})

test_that("crossref_verdict does not flag a notice via update-to metadata", {
  msg <- list(title = list("Retraction notice"),
              `update-to` = list(list(type = "retraction", DOI = "10.1/orig")))
  expect_false(crossref_verdict(msg, "10.1/notice")$matched)
  # But a RETRACTED: title prefix on the work itself still flags it.
  expect_true(crossref_verdict(list(title = list("RETRACTED: bad")), "10.1/x")$matched)
})

test_that("a fuzzy title match is never asserted as retracted", {
  hit <- build_xera_hit(
    list(retraction_nature = "Retraction", original_paper_doi = "10.1/x",
         record_id = "R", title = "T", retraction_date = "2015-01-01"),
    matched_on = "title", match_type = "title_fuzzy", confidence = 0.93
  )
  rec <- list(matched = TRUE, status = "retracted", confirming = "xera",
              checked = "xera", disagreement = FALSE, hit = hit, errored = FALSE)
  row <- finalize_row(as_reference(title = "T"), rec, list(flag_nature = DEFAULT_FLAG_NATURE))
  expect_equal(row$match_type, "title_fuzzy")
  expect_false(row$is_retracted)
})
