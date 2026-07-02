# These tests use the deterministic "testfake" backend registered in setup.R,
# so they never touch the network.

test_that("check_dois flags via a backend and detects identifiers", {
  res <- check_dois(c("10.1016/S0140-6736(97)11096-0", "10.1038/s41586-020-2649-2"),
                    sources = "testfake", resolve_ids = FALSE, progress = FALSE)
  expect_s3_class(res, "retraction_result")
  expect_equal(nrow(res), 2)
  expect_equal(sum(res$is_retracted), 1)
  expect_true(res$is_retracted[res$doi == "10.1016/s0140-6736(97)11096-0"])
  expect_equal(res$sources[res$is_retracted], "testfake")
})

test_that("check_refs auto-detects a doi column", {
  df <- data.frame(DOI = c("10.1016/S0140-6736(97)11096-0", "10.1/clean"),
                   stringsAsFactors = FALSE)
  res <- check_refs(df, sources = "testfake", resolve_ids = FALSE, progress = FALSE)
  expect_equal(sum(res$is_retracted), 1)
})

test_that("check_file parses a document and checks it", {
  res <- check_file(system.file("extdata", "dois.txt", package = "retraction"),
                    sources = "testfake", resolve_ids = FALSE, progress = FALSE)
  expect_gte(nrow(res), 1)
  expect_equal(sum(res$is_retracted), 1)
})

test_that("check_bib forces the bib parser", {
  res <- check_bib(system.file("extdata", "example.bib", package = "retraction"),
                   sources = "testfake", resolve_ids = FALSE, progress = FALSE)
  expect_equal(sum(res$is_retracted), 1)
})

test_that("offline without a snapshot is an informative error", {
  withr::local_envvar(R_USER_CACHE_DIR = tempfile("cache"))
  .snapshot_cache$data <- NULL
  expect_error(
    check_dois("10.1016/S0140-6736(97)11096-0", offline = TRUE,
               sources = "testfake", progress = FALSE),
    "snapshot"
  )
})

test_that("unknown source is rejected early", {
  expect_error(check_dois("10.1/x", sources = "does-not-exist", progress = FALSE),
               "Unknown source")
})
