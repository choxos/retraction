# Contributing to retraction

Thank you for helping improve `retraction`. The package scans documents and
bibliographies for citations to retracted publications, so changes should
preserve accuracy, reproducibility, and safe handling of user documents.

## Ways to contribute

Useful contributions include:

- Bug reports with a minimal reproducible example.
- Parser fixes or new format support, with a small sample file that shows the
  problem.
- Matching or source corrections with links to the relevant Retraction Watch,
  Crossref, or OpenAlex record.
- Tests for parsers, identifier normalization, matching, source backends, and
  report rendering.
- Documentation improvements for package users and contributors.
- Small, focused pull requests that are easy to review.

## Before opening an issue

Please search existing issues first. When reporting a bug, include:

- The file format or reference being checked, with a minimal sample if possible.
- The sources used, for example `"xera"`, `"crossref"`, or `"openalex"`.
- Whether the problem occurs offline (after `retraction_sync()`) or only against
  the live API.
- A short reproducible R example.
- Your `sessionInfo()` output.

Do not include private data, access tokens, or unpublished manuscripts in public
issues. If you must show a failing reference, reduce it to the smallest example.

## Development setup

From a local checkout:

```r
install.packages(c("devtools", "testthat"))
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
```

Run the package test suite with:

```r
testthat::test_local()
```

For a CRAN-style local check, build a clean source tarball and run:

```sh
R CMD build .
R CMD check --as-cran retraction_*.tar.gz
```

Optional features use packages in `Suggests`, including `pdftools` (PDF text
extraction), `RefManageR` (bibliography parsing), and `knitr` / `rmarkdown`
(vignettes and reports). If you change optional behavior, verify both the
installed-dependency path and the graceful-degradation path where practical.

## Code and data expectations

- Keep changes focused. Avoid mixing unrelated refactors with behavioral fixes.
- Add or update tests for parsing, normalization, matching, source, or report
  behavior that changes.
- Keep examples CRAN-safe. Network-dependent examples should use `\donttest{}`
  or `\dontrun{}`, and prefer the bundled `retraction_example` data or an offline
  snapshot where possible.
- Preserve provenance and credit for retraction data sources. Update
  `LICENSE.note` when data-source handling changes.
- Do not commit generated check directories, source tarballs, local caches, or
  credentials.
- Use clear branch names that describe the work.

## Pull requests

Open pull requests against `main`. A good pull request should include:

- A concise summary of the change.
- The reason the change is needed.
- Tests or checks that were run.
- Links to relevant issues, data-source records, or format specifications.

By contributing, you agree that your contribution will be distributed under the
project license.
