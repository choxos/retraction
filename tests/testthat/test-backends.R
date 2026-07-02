test_that("xera adapter maps live-shaped search items to a hit", {
  items <- readRDS(test_path("fixtures", "xera_search_wakefield.rds"))
  hit <- xera_items_to_hit(items, "10.1016/s0140-6736(97)11096-0")
  expect_true(hit$matched)
  expect_equal(hit$status, "retracted")
  expect_equal(hit$matched_on, "original_doi")
  expect_equal(hit$record_id, "4036")
  expect_s3_class(hit$retraction_date, "Date")
})

test_that("xera adapter returns NULL for a non-matching DOI", {
  items <- readRDS(test_path("fixtures", "xera_search_wakefield.rds"))
  expect_null(xera_items_to_hit(items, "10.9999/nope"))
})

test_that("crossref_verdict detects retraction signals", {
  expect_true(crossref_verdict(list(title = list("RETRACTED: bad")), "10.1/x")$matched)
  expect_true(crossref_verdict(
    list(title = list("ok"), relation = list("is-retracted-by" = list())), "10.1/x")$matched)
  expect_false(crossref_verdict(list(title = list("a fine paper")), "10.1/y")$matched)
})

test_that("openalex_verdict reads is_retracted", {
  expect_true(openalex_verdict(list(is_retracted = TRUE, display_name = "X"), "10.1/x")$matched)
  expect_false(openalex_verdict(list(is_retracted = FALSE, display_name = "X"), "10.1/x")$matched)
})

test_that("pick_governing prefers the most recent notice (reinstatement wins)", {
  a <- list(retraction_nature = "Retraction", retraction_date = "2010-01-01")
  b <- list(retraction_nature = "Reinstatement", retraction_date = "2012-01-01")
  gov <- pick_governing(list(a, b))
  expect_equal(classify_status(gov$retraction_nature), "reinstated")
})

test_that("reconcile_sources merges verdicts and flags disagreement", {
  h1 <- new_hit("xera", 1L, checked = TRUE, matched = TRUE, status = "retracted")
  h2 <- new_hit("openalex", 3L, checked = TRUE, matched = FALSE)
  rec <- reconcile_sources(list(h1, h2))
  expect_true(rec$matched)
  expect_equal(rec$status, "retracted")
  expect_equal(rec$confirming, "xera")
  expect_true(rec$disagreement)
})

test_that("reconcile_sources reports unchecked when nothing responded", {
  rec <- reconcile_sources(list(new_hit("xera", 1L, checked = FALSE)))
  expect_false(rec$matched)
})

test_that("resolve_sources validates names", {
  expect_error(resolve_sources("nope"), "Unknown source")
  expect_setequal(resolve_sources("all"), list_backends())
  expect_true("xera" %in% list_backends())
})
