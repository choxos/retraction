# Pure-function tests for the Europe PMC, NCBI, and DataCite verdicts.

test_that("europepmc_verdict flags a retracted publication", {
  r <- list(list(title = "A study",
                 pubTypeList = list(pubType = list("Retracted Publication", "Journal Article"))))
  expect_equal(europepmc_verdict(r, "10.1/x")$status, "retracted")
})

test_that("europepmc_verdict treats a retraction notice as matched but not flagged", {
  r <- list(list(title = "Retraction: A study",
                 pubTypeList = list(pubType = list("Retraction of Publication"))))
  v <- europepmc_verdict(r, "10.1/n")
  expect_equal(v$status, "none")
  expect_true(v$matched)
  expect_equal(v$matched_on, "retraction_doi")
})

test_that("europepmc_verdict detects an expression of concern via comments", {
  r <- list(list(title = "A study",
                 pubTypeList = list(pubType = list("Journal Article")),
                 commentCorrectionList = list(commentCorrection = list(
                   list(type = "Expression of concern in")))))
  expect_equal(europepmc_verdict(r, "10.1/x")$status, "expression_of_concern")
})

test_that("europepmc_verdict returns a clean checked hit for a normal article", {
  r <- list(list(title = "A study",
                 pubTypeList = list(pubType = list("Journal Article"))))
  v <- europepmc_verdict(r, "10.1/x")
  expect_false(v$matched)
  expect_true(v$checked)
})

test_that("ncbi_verdict maps publication types", {
  expect_equal(ncbi_verdict(c("journal article", "retracted publication"), "1", "10.1/x")$status,
               "retracted")
  notice <- ncbi_verdict("retraction of publication", "2", "10.1/n")
  expect_equal(notice$status, "none")
  expect_true(notice$matched)
  expect_false(ncbi_verdict("journal article", "3", "10.1/y")$matched)
})

test_that("datacite_verdict flags a retraction/obsoletion relation", {
  res <- list(data = list(attributes = list(
    titles = list(list(title = "A dataset")),
    relatedIdentifiers = list(list(relationType = "IsObsoletedBy")))))
  expect_equal(datacite_verdict(res, "10.5/d")$status, "retracted")
  clean <- list(data = list(attributes = list(titles = list(list(title = "A dataset")))))
  expect_false(datacite_verdict(clean, "10.5/d")$matched)
})
