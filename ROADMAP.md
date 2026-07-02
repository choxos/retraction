# retraction roadmap

`retraction` scans documents, bibliographies, and reference lists for citations
to retracted publications. This file tracks what is done and what is planned, so
nothing is forgotten.

Legend: `[x]` done · `[~]` partial · `[ ]` todo

## v0.1.0: first release  `[x]`

Input and parsing
- [x] `check_file()` detects a file's format and checks every reference it holds
- [x] Bibliography formats: BibTeX and BibLaTeX (`.bib`), CSL-JSON (`.json`), RIS (`.ris`), EndNote XML
- [x] Document formats: JATS XML, Word (`.docx`), PDF, and R Markdown, Quarto, LaTeX, Markdown, plain text, and HTML (`.Rmd`/`.qmd`/`.tex`/`.md`/`.txt`/`.html`), with DOIs scraped from the text
- [x] `check_bib()` for bibliography files, `check_dois()` for a vector of DOIs or PMIDs, `check_refs()` for a data frame with auto-detected columns

Identifiers
- [x] `normalize_doi()`, `normalize_pmid()`, and `normalize_title()` clean and canonicalize identifiers before matching

Sources and reconciliation
- [x] Pluggable sources: `"xera"` (Retraction Watch via the XeraRetractionTracker API, default), `"crossref"`, `"openalex"`; `list_backends()` lists them
- [x] Query several sources at once; the highest-priority match sets the verdict, every confirming source is recorded, and a disagreement flag is raised when sources do not agree

Matching
- [x] Strict cascade: exact DOI, then PMID (resolved to a DOI through OpenAlex), then fuzzy title matching for references without an identifier
- [x] Exact matches asserted with high confidence; fuzzy matches reported as possible; retraction notices and reinstated works handled correctly

Offline snapshot
- [x] `retraction_sync()` builds a local snapshot with incremental updates; `retraction_cache_dir()` and `retraction_clear_cache()` manage it
- [x] Offline checks warn when the snapshot falls behind the live database

Results and reports
- [x] `retraction_result` tibble with `print()`, `summary()`, `as.data.frame()`, and `as_tibble()` methods; `retracted()` returns the flagged rows
- [x] `render_report()` writes a self-contained HTML report or a Markdown report
- [x] Bundled `retraction_example` data for offline examples and tests

Infrastructure
- [x] GitHub Actions: `R-CMD-check` (mac/win/linux + devel), `pkgdown`, and `test-coverage`
- [x] pkgdown website, README, and a getting-started vignette

## Phase 1: Citation context  `[ ]`
- [ ] Distinguish acknowledged citations of a retraction (citing the retraction notice, or explicitly noting the retracted status) from unacknowledged citations that treat the work as valid
- [ ] Surface the surrounding sentence or citation context in the result and report so a reviewer can judge intent quickly

## Phase 2: Command line and continuous integration  `[ ]`
- [ ] A command line entry point that checks a file or directory and prints a report
- [ ] Meaningful process exit codes (nonzero when unacknowledged retracted citations are found) so a check can gate a build
- [ ] A reusable GitHub Action and a pre-commit hook that run the check on the manuscripts and bibliographies in a repository

## Phase 3: Interactive tools  `[ ]`
- [ ] An RStudio addin to check the active document or a selected bibliography
- [ ] A Shiny triage app to review flagged citations, inspect their context, and mark false positives

## Phase 4: Richer reporting  `[ ]`
- [ ] Suggest replacement papers for a retracted reference (later work by the same authors, or a corrected version where one exists)
- [ ] Citation-graph visualization showing how retracted work propagates through a bibliography

## Phase 5: More sources and matching  `[ ]`
- [ ] A PubMed / NCBI backend for direct PMID lookups and title search, reducing reliance on OpenAlex for PMID resolution
- [ ] Additional reconciliation tuning: per-source confidence weighting, configurable priority, and better handling of partial or conflicting metadata

## Cross-cutting  `[~]`
- [x] testthat suite for parsing, normalization, matching, sources, and reporting
- [x] CRAN-safe examples using bundled data and offline snapshots; network-dependent examples gated with `\donttest{}`
- [ ] Recorded HTTP fixtures so source-backend tests replay offline in CI
- [ ] A broader real-world corpus of sample documents for regression testing across formats

## Known limitations
- Fuzzy title and PMID matching are most reliable offline, where the full corpus is available locally; online, they depend on what each source API returns
- OpenAlex `is_retracted` is itself derived from Retraction Watch, so it is not a fully independent source
- Text-only formats (`.txt`, `.md`, scraped HTML) rely on DOIs appearing in the text; a reference without a DOI in the text can only be matched by title when title metadata is available
- PDF extraction quality depends on the source PDF and on `pdftools`
