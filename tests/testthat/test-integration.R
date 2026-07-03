# Live smoke tests. Skipped on CRAN and when the network is unavailable.

test_that("live Xera lookup flags a known retraction", {
  skip_on_cran()
  skip_if_offline("openscience.xera.ac")
  res <- check_dois("10.1016/S0140-6736(97)11096-0", sources = "xera",
                    resolve_ids = FALSE, progress = FALSE)
  expect_true(res$is_retracted[1])
  expect_equal(res$status[1], "retracted")
  expect_equal(res$matched_on[1], "original_doi")
})

test_that("live multi-source reconciliation agrees on a clear retraction", {
  skip_on_cran()
  skip_if_offline("openscience.xera.ac")
  res <- check_dois("10.1016/S0140-6736(97)11096-0",
                    sources = c("xera", "openalex"), resolve_ids = FALSE,
                    progress = FALSE)
  expect_true(res$is_retracted[1])
})

test_that("live PMID resolves to a DOI via OpenAlex", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  expect_equal(resolve_pmid_to_doi("9500320"), "10.1016/s0140-6736(97)11096-0")
})

test_that("live native PMID match flags a retraction without OpenAlex", {
  skip_on_cran()
  skip_if_offline("openscience.xera.ac")
  # resolve_ids = FALSE disables the OpenAlex fallback, so this exercises the
  # Xera PMID index directly.
  res <- check_dois("9500320", sources = "xera", resolve_ids = FALSE,
                    progress = FALSE)
  expect_true(res$is_retracted[1])
  expect_equal(res$matched_on[1], "pmid")
})

test_that("live suggest_alternatives returns a related record", {
  skip_on_cran()
  skip_if_offline("openscience.xera.ac")
  alt <- suggest_alternatives("10.1016/S0140-6736(97)11096-0")
  expect_true(is.null(alt) || nrow(alt) >= 1)
})
