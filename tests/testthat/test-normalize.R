test_that("normalize_doi strips resolvers and trailing punctuation", {
  expect_equal(normalize_doi("https://doi.org/10.1234/ABC. "), "10.1234/abc")
  expect_equal(normalize_doi("doi:10.1016/S0140-6736(97)11096-0"),
               "10.1016/s0140-6736(97)11096-0")
  expect_true(is.na(normalize_doi("not a doi")))
  expect_true(is.na(normalize_doi(NA)))
})

test_that("normalize_pmid keeps digits only", {
  expect_equal(normalize_pmid("PMID: 12345678"), "12345678")
  expect_equal(normalize_pmid(9876543), "9876543")
  expect_true(is.na(normalize_pmid("abc")))
})

test_that("normalize_title removes markers, markup, and punctuation", {
  expect_equal(normalize_title("RETRACTED: A Study!"), "a study")
  expect_equal(normalize_title("Expression of Concern: Foo <i>bar</i>"), "foo bar")
})

test_that("identifier sniffing works", {
  expect_true(looks_like_doi("10.1016/j.cell.2020.01.001"))
  expect_false(looks_like_doi("10.1/x"))
  expect_true(looks_like_pmid("12345678"))
  expect_false(looks_like_pmid("10.1/x"))
})
