test_that("score_match ranks exact above fuzzy and caps fuzzy", {
  expect_equal(score_match("doi_exact"), 1)
  expect_equal(score_match("pmid_exact"), 0.99)
  expect_lt(score_match("title_fuzzy", 0.99, 0, 2), 0.95)
  expect_gt(score_match("title_fuzzy", 0.99, 0, 2),
            score_match("title_fuzzy", 0.86, 3, 0))
})

test_that("classify_status maps notice natures", {
  expect_equal(classify_status("Retraction"), "retracted")
  expect_equal(classify_status("Expression of Concern"), "expression_of_concern")
  expect_equal(classify_status("Reinstatement"), "reinstated")
  expect_equal(classify_status("Correction"), "correction")
  expect_equal(classify_status(NA), "other")
})

test_that("flagging respects the flag_nature set", {
  expect_true(status_is_flagged("retracted"))
  expect_true(status_is_flagged("expression_of_concern"))
  expect_false(status_is_flagged("correction"))
  expect_false(status_is_flagged("retracted", flag_nature = "Expression of Concern"))
})
