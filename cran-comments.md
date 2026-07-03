## Submission

This is the first submission of 'retraction' to CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Ahmad Sofi-Mahmudi <a.sofimahmudi@gmail.com>'
  New submission

The only note is the standard "New submission" note. The CRAN incoming
spell-check may additionally list possibly-misspelled words in DESCRIPTION;
these are bibliographic-format names and proper nouns (BibLaTeX, CSL, EndNote,
JATS, RIS, XeraRetractionTracker, arXiv, bioRxiv, DataCite) plus the correctly
spelled word "lookups", all intentional.

## Test environments

* local: macOS, R 4.6.0
* GitHub Actions: macOS (release), Windows (release), Ubuntu (release),
  Ubuntu (devel)

Full `R CMD check --as-cran` was run on the built tarball with all suggested
packages installed.

## Notes for CRAN

* The package queries public web APIs (the Retraction Watch database served
  through the XeraRetractionTracker API, and optionally Crossref, OpenAlex,
  Europe PMC, PubMed/NCBI, DataCite, and arXiv/bioRxiv). Every example that
  accesses the network is wrapped in `\donttest{}` (or `\dontrun{}` for examples
  that also need local state such as a cache, a Zotero database, or RStudio).
  All network access fails gracefully: the HTTP layer returns `NULL` on any
  transport error or non-2xx response, so an unreachable service yields an
  "unchecked" result rather than an error or warning.

* The test suite does not access the network during `R CMD check`. Live smoke
  tests are skipped with `skip_on_cran()`; the remaining online code paths are
  covered by offline tests that replay recorded HTTP fixtures.

* The package does not write to the user's home filespace, the package
  directory, or other prohibited locations. The optional local snapshot and any
  cached PubMed Central XML are written under
  `tools::R_user_dir("retraction", "cache")`, and only when the user explicitly
  calls `retraction_sync()` or `check_pmc()`.
