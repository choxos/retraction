ex <- function(f) system.file("extdata", f, package = "retraction")

test_that("detect_format infers by extension and XML sniff", {
  expect_equal(detect_format("a.bib"), "bib")
  expect_equal(detect_format("a.ris"), "ris")
  expect_equal(detect_format("a.json"), "csljson")
  expect_equal(detect_format("a.docx"), "docx")
  expect_equal(detect_format(ex("example_jats.xml")), "jats")
  expect_equal(detect_format(ex("example_endnote.xml")), "endnote")
})

test_that("parse_bib extracts identifiers, titles, and keys", {
  refs <- parse_bib(ex("example.bib"))
  expect_length(refs, 3)
  expect_equal(refs[[1]]$id, "wakefield1998")
  expect_equal(refs[[1]]$doi, "10.1016/s0140-6736(97)11096-0")
  expect_true(is.na(refs[[3]]$doi))
  expect_equal(refs[[3]]$input_type, "title")
})

test_that("parse_csljson reads DOI, title, year, author", {
  refs <- parse_csljson(ex("example.json"))
  expect_length(refs, 2)
  expect_equal(refs[[1]]$doi, "10.1016/s0140-6736(97)11096-0")
  expect_equal(refs[[1]]$year, 1998L)
  expect_match(refs[[1]]$author, "Wakefield")
})

test_that("parse_ris reads records", {
  refs <- parse_ris(ex("example.ris"))
  expect_length(refs, 2)
  expect_equal(refs[[2]]$doi, "10.1038/s41586-020-2649-2")
  expect_equal(refs[[1]]$year, 1998L)
})

test_that("parse_jats reads reference-list DOIs", {
  refs <- parse_jats(ex("example_jats.xml"))
  dois <- vapply(refs, function(r) r$doi %||% NA_character_, character(1))
  expect_true("10.1016/s0140-6736(97)11096-0" %in% dois)
})

test_that("parse_endnote reads records", {
  refs <- parse_endnote(ex("example_endnote.xml"))
  dois <- vapply(refs, function(r) r$doi %||% NA_character_, character(1))
  expect_true("10.1016/s0140-6736(97)11096-0" %in% dois)
})

test_that("text scraping finds DOIs with line provenance", {
  refs <- parse_text(ex("example.Rmd"))
  dois <- vapply(refs, function(r) r$doi, character(1))
  expect_true("10.1016/s0140-6736(97)11096-0" %in% dois)
  expect_true(all(!is.na(vapply(refs, function(r) r$source_file, character(1)))))
})

test_that("strip_wrapping drops unbalanced brackets but keeps DOI parens", {
  expect_equal(strip_wrapping("10.1000/xyz)"), "10.1000/xyz")
  expect_equal(strip_wrapping("10.1016/S0140-6736(97)11096-0"),
               "10.1016/S0140-6736(97)11096-0")
})
