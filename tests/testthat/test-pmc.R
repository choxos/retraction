test_that("normalize_pmcid canonicalizes to PMC####### or NA", {
  expect_equal(
    normalize_pmcid(c("PMC5334499", "5334499", "pmc 5334499", "x", NA)),
    c("PMC5334499", "PMC5334499", "PMC5334499", NA, NA)
  )
})

test_that("pmc_articles on an empty result is an empty tibble", {
  a <- pmc_articles(new_retraction_result(list()))
  expect_s3_class(a, "tbl_df")
  expect_equal(nrow(a), 0)
  expect_true(all(c("input", "pmcid", "doi", "resolved", "retrieved",
                    "is_open_access", "n_references", "n_retracted") %in% names(a)))
})

test_that("check_pmc with no input warns and returns an empty result", {
  expect_warning(res <- check_pmc(character(0), progress = FALSE))
  expect_s3_class(res, "retraction_result")
  expect_equal(nrow(res), 0)
})

test_that("live: check_pmc resolves a DOI to an open-access PMC article", {
  skip_on_cran()
  skip_if_offline("www.ncbi.nlm.nih.gov")
  res <- check_pmc("10.1371/journal.pone.0000217", sources = "xera",
                   resolve_ids = FALSE, progress = FALSE)
  a <- pmc_articles(res)
  skip_if(nrow(a) == 0 || is.na(a$pmcid[1]), "PMC resolution unavailable")
  expect_equal(a$pmcid[1], "PMC1790863")
  expect_true(a$is_open_access[1])
  expect_gt(a$n_references[1], 0)
})

test_that("live: pmc_fetch_xml returns a document with a reference list", {
  skip_on_cran()
  skip_if_offline("eutils.ncbi.nlm.nih.gov")
  doc <- pmc_fetch_xml("PMC1790863", cache = FALSE)
  skip_if(is.null(doc), "PMC article not retrievable")
  refs <- parse_jats_doc(doc, "PMC1790863")
  expect_gt(length(refs), 0)
})
