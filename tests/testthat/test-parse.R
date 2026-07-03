ex <- function(f) system.file("extdata", f, package = "retraction")

test_that("detect_format infers by extension and XML sniff", {
  expect_equal(detect_format("a.bib"), "bib")
  expect_equal(detect_format("a.ris"), "ris")
  expect_equal(detect_format("a.json"), "csljson")
  expect_equal(detect_format("a.docx"), "docx")
  expect_equal(detect_format("a.nxml"), "jats")
  expect_equal(detect_format(ex("example_jats.xml")), "jats")
  expect_equal(detect_format(ex("example_endnote.xml")), "endnote")
})

test_that("parse_jats_doc extracts references from a parsed document", {
  doc <- xml2::read_xml(ex("example_jats.xml"))
  refs <- parse_jats_doc(doc, source_file = "PMC_TEST")
  dois <- vapply(refs, function(r) r$doi %||% NA_character_, character(1))
  expect_true("10.1016/s0140-6736(97)11096-0" %in% dois)
  expect_equal(refs[[1]]$source_file, "PMC_TEST")
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

test_that("a DOI inside a Markdown link is not over-captured", {
  tf <- tempfile(fileext = ".md")
  writeLines("See [10.1234/abc](https://doi.org/10.1234/abc).", tf)
  dois <- vapply(parse_text(tf), function(r) r$doi, character(1))
  expect_true("10.1234/abc" %in% dois)
  expect_false(any(grepl("[][(){}]", dois)))
})

test_that("parse_bib reads a BibLaTeX date field for the year", {
  tf <- tempfile(fileext = ".bib")
  writeLines(c("@article{k,", "  title = {T},", "  date = {2020-03-01},",
               "  doi = {10.1/x}", "}"), tf)
  refs <- parse_bib(tf)
  expect_equal(refs[[1]]$year, 2020L)
})

test_that("parse_jats_doc scrapes a DOI from a mixed-citation text node", {
  doc <- xml2::read_xml(paste0(
    "<ref-list><ref><mixed-citation>Smith. Title. doi:10.7777/mixed. ",
    "2020.</mixed-citation></ref></ref-list>"))
  refs <- parse_jats_doc(doc, "x")
  expect_equal(refs[[1]]$doi, "10.7777/mixed")
})
