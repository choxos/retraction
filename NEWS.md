# retraction 0.1.0

First release. `retraction` scans manuscripts, bibliographies, and reference
lists for citations to retracted publications, so authors can find and remove
citations to retracted work before submitting. It reads a wide range of document
and bibliography formats, extracts and normalizes identifiers, checks them
against retraction data, and returns a tidy, confidence-scored report.

## Checking documents and bibliographies

* `check_file()` detects the format of a file and checks every reference it
  contains. Bibliography formats: BibTeX and BibLaTeX (`.bib`), CSL-JSON
  (`.json`), RIS (`.ris`), and EndNote XML. Document formats: JATS XML, Word
  (`.docx`), PDF, and R Markdown, Quarto, LaTeX, Markdown, plain text, and HTML
  (`.Rmd`, `.qmd`, `.tex`, `.md`, `.txt`, `.html`), from which DOIs are scraped
  from the text.
* `check_bib()` checks a bibliography file when you want to name the input
  explicitly.
* `check_dois()` checks a vector of DOIs or PMIDs directly.
* `check_refs()` checks a data frame of references, with identifier and title
  columns auto-detected.
* `normalize_doi()`, `normalize_pmid()`, and `normalize_title()` clean and
  canonicalize identifiers so equivalent forms match.

## Sources and reconciliation

* Retraction data comes from pluggable sources: `"xera"` (Retraction Watch via
  the XeraRetractionTracker API, the default), `"crossref"`, and `"openalex"`.
  `list_backends()` lists them.
* Any `check_*()` call can query several sources at once. The highest-priority
  match sets the verdict, every confirming source is recorded, and a
  disagreement flag is raised when sources do not agree.

## Matching

* Matching runs a strict cascade: exact DOI, then PMID (resolved to a DOI
  through OpenAlex, since the Retraction Watch API cannot be queried by PMID),
  then fuzzy title matching for references that carry no identifier.
* Exact identifier matches are asserted with high confidence; fuzzy matches are
  reported as possible so you can verify them. A citation of a retraction notice
  is not flagged, and a work that was later reinstated is reported as reinstated.

## Offline snapshot

* `retraction_sync()` downloads a local snapshot of the retraction corpus for
  bulk checking, privacy, and offline use. Updates are incremental, adding new
  retractions rather than re-downloading everything.
* `retraction_cache_dir()` reports where the snapshot lives, and
  `retraction_clear_cache()` removes it. Offline checks warn when the snapshot
  has fallen behind the live database.

## Results and reports

* A check returns a `retraction_result`: a tidy tibble, one row per reference,
  with the retraction status, an `is_retracted` flag, a match confidence, the
  retraction date, the reason, and which sources confirmed it. It has `print()`,
  `summary()`, `as.data.frame()`, and `as_tibble()` methods, and `retracted()`
  returns just the flagged rows.
* `render_report()` writes a self-contained HTML report, or a Markdown report
  with `format = "md"`.
* A bundled example, `retraction_example`, lets examples and tests run without
  network access.
