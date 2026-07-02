make_row <- function(...) {
  base <- list(
    id = NA_character_, input_type = NA_character_, query = NA_character_,
    doi = NA_character_, pmid = NA_character_, matched = FALSE, status = "none",
    is_retracted = FALSE, confidence = 0, match_type = NA_character_,
    matched_on = NA_character_, nature = NA_character_, record_id = NA_character_,
    matched_title = NA_character_, journal = NA_character_,
    retraction_date = as.Date(NA), days_since_retraction = NA_integer_,
    reason = NA_character_, sources = NA_character_, disagreement = FALSE,
    source_file = NA_character_, location = NA_character_
  )
  utils::modifyList(base, list(...))
}

test_that("new_retraction_result builds a typed, classed tibble", {
  res <- new_retraction_result(list(
    make_row(id = "10.1/a", doi = "10.1/a", matched = TRUE, status = "retracted",
             is_retracted = TRUE, confidence = 1, matched_title = "Bad paper",
             retraction_date = as.Date("2010-01-01"), sources = "xera"),
    make_row(id = "10.1/b", doi = "10.1/b")
  ))
  expect_s3_class(res, "retraction_result")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_type(res$is_retracted, "logical")
  expect_s3_class(res$retraction_date, "Date")
  expect_equal(sum(res$is_retracted), 1)
})

test_that("empty result has the right shape", {
  res <- new_retraction_result(list())
  expect_s3_class(res, "retraction_result")
  expect_equal(nrow(res), 0)
  expect_true(all(RESULT_COLS %in% names(res)))
})

test_that("retracted() subsets and keeps class", {
  res <- new_retraction_result(list(
    make_row(id = "a", matched = TRUE, status = "retracted", is_retracted = TRUE, confidence = 1),
    make_row(id = "b")
  ))
  fl <- retracted(res)
  expect_s3_class(fl, "retraction_result")
  expect_equal(nrow(fl), 1)
})

test_that("print and summary work", {
  res <- new_retraction_result(list(
    make_row(id = "10.1/a", doi = "10.1/a", matched = TRUE, status = "retracted",
             is_retracted = TRUE, confidence = 1, matched_title = "Bad paper")
  ))
  expect_invisible(out <- print(res))
  expect_identical(out, res)
  s <- summary(res)
  expect_equal(s$n[s$metric == "flagged"], 1)
})

test_that("render_report writes HTML and Markdown", {
  res <- new_retraction_result(list(
    make_row(id = "10.1/a", doi = "10.1/a", matched = TRUE, status = "retracted",
             is_retracted = TRUE, confidence = 1, matched_title = "Bad paper",
             retraction_date = as.Date("2010-01-01"), reason = "Fabrication")
  ))
  html <- render_report(res, tempfile(fileext = ".html"))
  expect_true(file.exists(html))
  expect_gt(file.size(html), 0)
  expect_match(paste(readLines(html), collapse = ""), "Flagged citations")

  md <- render_report(res, tempfile(fileext = ".md"), format = "md")
  expect_match(paste(readLines(md), collapse = "\n"), "Flagged citations")
})
