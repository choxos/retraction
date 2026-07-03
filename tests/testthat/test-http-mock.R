# Offline tests of the online code paths, replaying recorded HTTP fixtures via
# httptest2. The fixtures (mock-* directories) are recorded once against the live
# APIs and replayed with no network. They are build-ignored (their URL-derived
# paths exceed the tarball path limit), so these run from the source tree
# (devtools::test(), CI) and skip when the fixtures are absent.

mock_or_skip <- function(dir) {
  skip_if_not_installed("httptest2")
  skip_if(!dir.exists(testthat::test_path(dir)), "recorded fixtures not available")
}

WAKEFIELD <- "10.1016/S0140-6736(97)11096-0"
WAKEFIELD_PMID <- "9500320"

test_that("xera flags a retraction by DOI (mocked)", {
  mock_or_skip("mock-xera-doi")
  httptest2::with_mock_dir(test_path("mock-xera-doi"), {
    res <- check_dois(WAKEFIELD, sources = "xera", resolve_ids = FALSE, progress = FALSE)
    expect_true(res$is_retracted[1])
    expect_equal(res$matched_on[1], "original_doi")
  })
})

test_that("xera flags a retraction by native PMID (mocked)", {
  mock_or_skip("mock-xera-pmid")
  httptest2::with_mock_dir(test_path("mock-xera-pmid"), {
    res <- check_dois(WAKEFIELD_PMID, sources = "xera", resolve_ids = FALSE, progress = FALSE)
    expect_true(res$is_retracted[1])
    expect_equal(res$matched_on[1], "pmid")
  })
})

test_that("crossref flags a retraction (mocked)", {
  mock_or_skip("mock-crossref")
  httptest2::with_mock_dir(test_path("mock-crossref"), {
    res <- check_dois(WAKEFIELD, sources = "crossref", resolve_ids = FALSE, progress = FALSE)
    expect_true(res$is_retracted[1])
  })
})

test_that("openalex flags a retraction (mocked)", {
  mock_or_skip("mock-openalex")
  httptest2::with_mock_dir(test_path("mock-openalex"), {
    res <- check_dois(WAKEFIELD, sources = "openalex", resolve_ids = FALSE, progress = FALSE)
    expect_true(res$is_retracted[1])
  })
})

test_that("europepmc flags a retraction (mocked)", {
  mock_or_skip("mock-europepmc")
  httptest2::with_mock_dir(test_path("mock-europepmc"), {
    res <- check_dois(WAKEFIELD, sources = "europepmc", resolve_ids = FALSE, progress = FALSE)
    expect_true(res$is_retracted[1])
  })
})

test_that("ncbi flags a retraction via PubMed types (mocked)", {
  mock_or_skip("mock-ncbi")
  httptest2::with_mock_dir(test_path("mock-ncbi"), {
    res <- check_dois(WAKEFIELD, sources = "ncbi", resolve_ids = FALSE, progress = FALSE)
    expect_true(res$is_retracted[1])
  })
})

test_that("datacite reports not-applicable for a non-DataCite DOI (mocked)", {
  mock_or_skip("mock-datacite")
  httptest2::with_mock_dir(test_path("mock-datacite"), {
    res <- check_dois(WAKEFIELD, sources = "datacite", resolve_ids = FALSE, progress = FALSE)
    expect_false(res$is_retracted[1])
  })
})

test_that("check_preprint reads bioRxiv (mocked)", {
  mock_or_skip("mock-preprint")
  httptest2::with_mock_dir(test_path("mock-preprint"), {
    pp <- check_preprint("10.1101/2020.01.30.927871")
    expect_equal(pp$server, "biorxiv")
  })
})

test_that("check_pmc resolves and reads an article (mocked)", {
  mock_or_skip("mock-pmc")
  httptest2::with_mock_dir(test_path("mock-pmc"), {
    res <- check_pmc(WAKEFIELD, sources = "xera", progress = FALSE)
    expect_s3_class(res, "retraction_result")
  })
})
