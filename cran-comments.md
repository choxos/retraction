## Submission

This is the first submission of 'retraction' to CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* The NOTE additionally lists possibly-misspelled words in DESCRIPTION. These
  are bibliographic format names and proper nouns (BibLaTeX, CSL, EndNote,
  JATS, RIS, XeraRetractionTracker) plus the correctly-spelled word "lookups".

## Test environments

* local macOS, R 4.6.0
* win-builder (R release 4.6.1 and R-devel)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release,
  oldrel-1)

## Notes for CRAN

* The package queries public web APIs (the Retraction Watch database served
  through the XeraRetractionTracker API, and optionally Crossref and OpenAlex).
  Every example that accesses the network is wrapped in `\donttest{}` or guarded
  with `if (interactive())`. The test suite does not access the network during
  `R CMD check`: live smoke tests use `skip_on_cran()` and `skip_if_offline()`.

* The package does not write to the user's home filespace, the package
  directory, or other prohibited locations. The optional local snapshot is
  written under `tools::R_user_dir("retraction", "cache")`, and only when the
  user explicitly calls `retraction_sync()`.
