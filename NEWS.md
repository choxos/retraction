# retraction 0.1.0

First release. `retraction` scans manuscripts, bibliographies, and reference
lists for citations to retracted publications, so authors can find and remove
citations to retracted work before submitting. It reads a wide range of document
and bibliography formats, extracts and normalizes identifiers, checks them
against retraction data, and returns a tidy, scored report.

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
* `check_pmc()` accepts a PMID, PMCID, DOI, title, or whole reference string,
  resolves it to a PubMed Central article, reports whether the open-access full
  text is available, and if so checks the article's reference list for
  retractions. `pmc_articles()` returns the per-article open-access summary, and
  `pmc_fetch_xml()` retrieves the open-access JATS XML directly.
* `normalize_doi()`, `normalize_pmid()`, `normalize_pmcid()`, and
  `normalize_title()` clean and canonicalize identifiers so equivalent forms
  match.

## Sources and reconciliation

* Retraction data comes from pluggable sources: `"xera"` (Retraction Watch via
  the XeraRetractionTracker API, the default), `"crossref"`, `"openalex"`,
  `"europepmc"`, `"ncbi"` (PubMed), `"datacite"`, and `"preprint"` (arXiv and
  bioRxiv withdrawals). `list_backends()` lists them; `sources = "all"` queries
  every one.
* Any `check_*()` call can query several sources at once. The highest-priority
  match sets the verdict, every confirming source is recorded, and a
  disagreement flag is raised when sources do not agree.
* The Crossref source now recognizes corrections and expressions of concern
  (via `update-to`), not only retractions.

## Matching

* Matching runs a strict cascade: exact DOI, then PMID (matched directly against
  the Retraction Watch corpus, falling back to OpenAlex only to obtain a DOI for
  the other sources), then fuzzy title matching for references without an
  identifier. PMID matching no longer requires OpenAlex and also works offline.
* Exact identifier matches are asserted with high confidence; fuzzy matches are
  reported as possible so you can verify them. A citation of a retraction notice
  is not flagged, and a work that was later reinstated is reported as reinstated.
* Title normalization now folds accents, ligatures, full-width forms, and
  non-Latin scripts (via stringi), so non-English titles match more reliably.

## More inputs, scale, and interfaces

* `check_zotero()` scans a Zotero library directly from its database.
* `check_preprint()` reports whether an arXiv or bioRxiv preprint was withdrawn.
* `retraction_app()` launches a Shiny triage app to upload a file and browse
  results interactively.
* The HTML report is now sortable and filterable in the browser (self-contained,
  no external assets).
* Offline matching uses an in-memory hash index for O(1) DOI lookups;
  `retraction_snapshot_parquet()` exports the corpus for arrow-based analysis;
  and checking parallelizes across references when a `future` plan is set.

## Offline snapshot

* `retraction_sync()` downloads a local snapshot of the retraction corpus for
  bulk checking, privacy, and offline use. Updates are incremental, adding new
  retractions rather than re-downloading everything.
* `retraction_cache_dir()` reports where the snapshot lives, and
  `retraction_clear_cache()` removes it. Offline mode is fully local by default;
  an optional notice (`options(retraction.check_freshness = TRUE)`) warns when
  the snapshot has fallen behind the live database.

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

## Interpreting results

* `explain_result()` gives a plain-language sentence per reference: what matched,
  on which identifier, at what confidence, which sources confirmed, and any
  disagreement.
* `compare_sources()` returns the rows where the selected sources disagreed.
* `exposure_score()` summarizes a document's retraction exposure with proper
  denominators (checked, unchecked, possible), not a bare flagged rate.
* `classify_timing()` labels each citation relative to the document's date
  (conservatively, `document_after_retraction`, unless you supply per-citation
  dates), to distinguish work cited before vs after its retraction.
* `snapshot_info()` reports which retraction-database version an offline check
  ran against; `badge_json()` writes a shields.io endpoint for a README badge.

## Monitoring and systematic reviews

* `retraction_watch_save()` / `retraction_watch_diff()` register a bibliography
  and later report references that have *become* retracted since, keyed on
  normalized identifiers so re-ordering does not confuse the diff.
* `check_included_studies()` checks a review's included-study identifiers,
  deduplicating and reporting checked/unchecked/retracted counts, since a
  retracted included trial can invalidate a pooled estimate.

## Workflow integration

* `retraction_scan()` and `retraction_main()` power a command-line check that
  exits non-zero per a `fail_policy()` (`flagged`, `possible`, `unchecked`,
  `error`) and **fails closed**: a missing file or a fetch error is an error,
  never a silently clean pass.
* `retraction_knit_check()` gates a knitr/Quarto render on retracted citations.
* An RStudio addin checks the active document; a ready-made GitHub Action
  (`inst/actions/action.yml`) fails CI on retracted citations.
* Every `check_*()` gains `strict = TRUE`, which errors when any reference could
  not be checked rather than returning it as `unchecked`.

## Export, annotation, and queries

* `export_result()` writes CSV, JSON, or Excel; `annotate_bib()` writes a
  bibliography back out with retracted entries marked (idempotently).
* `suggest_alternatives()` returns the records the corpus links to a retracted
  work (a correction or reinstatement) to help decide what to cite instead.
* `author_retractions()` and `journal_retractions()` query the corpus by author
  or journal; `primary_reason_bucket()` / `reason_buckets()` group free-text
  retraction reasons into a coarse taxonomy.
* Title matching adds a strict `title_exact` tier: an exact title, year, and
  first author (with a short-title guard) is asserted rather than only flagged
  as "possible".
