# Preprint classification and withdrawal-text detection (pure functions).

test_that("preprint_ref classifies arXiv and bioRxiv identifiers", {
  expect_equal(preprint_ref(id = "2401.01234")$server, "arxiv")
  expect_equal(preprint_ref(id = "arXiv:2401.01234")$server, "arxiv")
  expect_equal(preprint_ref(doi = "10.48550/arXiv.2401.01234")$server, "arxiv")
  expect_equal(preprint_ref(doi = "10.48550/arXiv.2401.01234")$id, "2401.01234")
  bx <- preprint_ref(doi = "10.1101/2020.01.30.927871")
  expect_equal(bx$server, "biorxiv")
  expect_null(preprint_ref(doi = "10.1016/j.cell.2020.01.001"))
  expect_null(preprint_ref(id = "not-an-id"))
})

test_that("is_withdrawn_text detects withdrawal language", {
  expect_true(is_withdrawn_text("This paper has been withdrawn by the authors."))
  expect_true(is_withdrawn_text(NA, "withdrawn"))
  expect_false(is_withdrawn_text("A normal abstract about mitochondria."))
  expect_false(is_withdrawn_text(NA, NA))
})

test_that("backend_preprint ignores a non-preprint reference", {
  hit <- backend_preprint(as_reference(doi = "10.1016/j.cell.2020.01.001"),
                          list(offline = FALSE))
  expect_false(hit$matched)
  expect_equal(hit$state, "not_applicable")
})
