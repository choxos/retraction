# Zotero long-to-wide pivot (pure; no database needed).

test_that("zotero_items_to_df pivots one row per item", {
  rows <- data.frame(
    itemID = c(1L, 1L, 1L, 2L, 2L),
    field = c("DOI", "title", "date", "title", "DOI"),
    value = c("10.1/x", "A paper", "2010-05-01", "Another", "10.2/y"),
    stringsAsFactors = FALSE
  )
  df <- zotero_items_to_df(rows)
  expect_equal(nrow(df), 2L)
  expect_setequal(df$doi, c("10.1/x", "10.2/y"))
  expect_equal(df$year[df$doi == "10.1/x"], "2010")
})

test_that("zotero_items_to_df handles an empty library", {
  df <- zotero_items_to_df(data.frame(itemID = integer(), field = character(),
                                      value = character()))
  expect_equal(nrow(df), 0L)
  expect_setequal(names(df), c("doi", "title", "year"))
})
