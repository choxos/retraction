# Input parsing. Each parser returns a list of normalized references (see
# `as_reference()`), tagged with their source file and, where available, a
# location (line number or page). Formats split into structured (identifiers
# preserved) and heuristic (DOIs scraped from text).

# A permissive DOI pattern; trailing prose punctuation is trimmed afterwards.
# Brackets and braces are excluded so a DOI written inside a Markdown link or a
# LaTeX macro (e.g. `[10.x/y](https://doi.org/10.x/y)`) is not over-captured.
# Parentheses stay allowed because DOIs legitimately contain them. The left
# lookbehind avoids matching a `10.` that is glued to a preceding letter, digit,
# or dot (e.g. version strings), reducing false DOIs scraped from prose. DOI
# scraping remains best-effort: a URL path segment shaped like a DOI can still
# match, but such a false DOI simply resolves to "not retracted".
DOI_PATTERN <- "(?<![0-9A-Za-z.])10\\.[0-9]{4,9}/[^[:space:]\"'<>\\]\\[{}]+"

#' Strip prose wrapping from a scraped DOI token.
#'
#' Removes trailing sentence punctuation and trailing brackets, but only strips
#' a closing bracket when it is unbalanced, so DOIs that legitimately contain
#' parentheses (e.g. `S0140-6736(97)11096-0`) are preserved.
#' @noRd
strip_wrapping <- function(x) {
  vapply(x, function(s) {
    repeat {
      s2 <- sub("[.,;:]+$", "", s, perl = TRUE)
      if (grepl("[)\\]}]$", s2, perl = TRUE)) {
        opens <- nchar(gsub("[^(\\[{]", "", s2, perl = TRUE))
        closes <- nchar(gsub("[^)\\]}]", "", s2, perl = TRUE))
        if (closes > opens) s2 <- sub("[)\\]}]$", "", s2, perl = TRUE)
      }
      if (identical(s2, s)) break
      s <- s2
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

#' Extract DOIs from a character vector, keeping line indices.
#' @return A list of `list(doi, line)`.
#' @noRd
extract_dois <- function(lines) {
  out <- list()
  hits <- regmatches(lines, gregexpr(DOI_PATTERN, lines, perl = TRUE))
  for (i in seq_along(hits)) {
    m <- hits[[i]]
    if (length(m)) {
      m <- strip_wrapping(m)
      for (d in m) if (nzchar(d)) out[[length(out) + 1L]] <- list(doi = d, line = i)
    }
  }
  out
}

#' Drop references whose (nonempty) identifier repeats an earlier one.
#' @noRd
dedup_refs <- function(refs) {
  if (!length(refs)) return(refs)
  keys <- vapply(refs, function(r) r$doi %||% r$pmid %||% "", character(1))
  refs[!(duplicated(keys) & nzchar(keys))]
}

#' Scrape DOIs from lines of text, one match at a time (per line).
#'
#' DOIs are matched per line rather than across a joined blob: joining separate
#' lines would concatenate distinct DOIs into one bogus identifier.
#' @noRd
doi_refs_from_lines <- function(lines, path, loc_prefix = NULL) {
  ds <- extract_dois(lines)
  refs <- lapply(ds, function(d) {
    loc <- if (!is.null(loc_prefix)) loc_prefix
           else if (!is.na(d$line)) d$line else NA
    as_reference(doi = d$doi, source_file = path, location = loc,
                 input_type = "doi")
  })
  dedup_refs(refs)
}

## ---------------------------------------------------------------------------
## Format detection and dispatch
## ---------------------------------------------------------------------------

#' Sniff an XML file to tell JATS from EndNote from generic XML.
#' @noRd
sniff_xml <- function(path) {
  txt <- tryCatch(paste(readLines(path, n = 80L, warn = FALSE), collapse = "\n"),
                  error = function(e) "")
  if (grepl("article-id|article-meta|<ref-list|JATS|Journal Archiving", txt,
            ignore.case = TRUE)) return("jats")
  if (grepl("<records>|<record>|EndNote|<rec-number>|electronic-resource-num", txt,
            ignore.case = TRUE)) return("endnote")
  "xmltext"
}

#' Detect the parser to use for a path.
#' @noRd
detect_format <- function(path, format = NULL) {
  if (!is.null(format)) return(format)
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    bib = "bib", bibtex = "bib",
    json = "csljson",
    ris = "ris", nbib = "ris",
    csv = "csv", tsv = "csv",
    docx = "docx",
    pdf = "pdf",
    xml = sniff_xml(path),
    nxml = "jats",
    "text"
  )
}

#' Parse an input file into a list of references.
#' @noRd
parse_input <- function(path, format = NULL) {
  if (!file.exists(path)) {
    cli::cli_warn("File not found: {.file {path}}.")
    return(list())
  }
  fmt <- detect_format(path, format)
  switch(
    fmt,
    bib = parse_bib(path),
    csljson = parse_csljson(path),
    csv = parse_csv(path),
    ris = parse_ris(path),
    endnote = parse_endnote(path),
    jats = parse_jats(path),
    docx = parse_docx(path),
    pdf = parse_pdf(path),
    parse_text(path)  # "text", "xmltext", and anything unrecognized
  )
}

## ---------------------------------------------------------------------------
## Text-like documents (Rmd, qmd, tex, md, txt, html, generic xml)
## ---------------------------------------------------------------------------

#' @noRd
parse_text <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  doi_refs_from_lines(lines, path)
}

## ---------------------------------------------------------------------------
## BibTeX / BibLaTeX
## ---------------------------------------------------------------------------

# BibTeX entry types that carry a citation (not @string/@comment/@preamble).
BIB_ENTRY_TYPES <- c(
  "article", "book", "booklet", "conference", "inbook", "incollection",
  "inproceedings", "manual", "mastersthesis", "misc", "phdthesis",
  "proceedings", "techreport", "unpublished", "online", "electronic",
  "dataset", "thesis", "report", "software"
)

#' Clean a raw bib field value (braced, quoted, or bare).
#' @noRd
clean_bib_value <- function(v) {
  v <- trimws(v)
  if (startsWith(v, "{")) {
    v <- extract_braced(v)
  } else if (startsWith(v, "\"")) {
    v <- sub("^\"([^\"]*)\".*", "\\1", v, perl = TRUE)
  }
  v <- gsub("[{}]", "", v)
  v <- gsub("[[:space:]]+", " ", v)
  na_if_empty(trimws(v))
}

#' Parse one bib entry's top-level fields, respecting brace/quote nesting.
#'
#' Splits on commas at brace depth 0 only, so a `doi = ` written inside another
#' field's braced value is never mistaken for the entry's DOI field.
#' @return A named list of field values (names lowercased).
#' @noRd
bib_fields <- function(entry) {
  body <- sub("^@[[:alpha:]]+[[:space:]]*\\{", "", entry, perl = TRUE)
  body <- sub("[[:space:]]*\\}[[:space:]]*$", "", body)
  chars <- strsplit(body, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (!n) return(list())
  depth <- 0L; inq <- FALSE; cut <- integer(0)
  for (i in seq_len(n)) {
    ch <- chars[i]
    if (ch == "\"") {
      inq <- !inq
      next
    }
    if (inq) next
    if (ch == "{") depth <- depth + 1L
    else if (ch == "}") depth <- depth - 1L
    else if (ch == "," && depth == 0L) cut <- c(cut, i)
  }
  bounds <- c(0L, cut, n + 1L)
  fields <- list()
  # Skip the first part (the citation key); the rest are "name = value".
  for (k in seq(2L, length(bounds) - 1L)) {
    lo <- bounds[k] + 1L; hi <- bounds[k + 1L] - 1L
    if (hi < lo) next
    part <- paste(chars[lo:hi], collapse = "")
    m <- regmatches(part, regexec("^\\s*([[:alnum:]_:.+-]+)\\s*=\\s*(.*)$", part,
                                  perl = TRUE))[[1]]
    if (length(m) == 3L) fields[[tolower(trimws(m[2L]))]] <- clean_bib_value(m[3L])
  }
  fields
}

#' Return the substring inside a leading `{...}`, honoring nested braces.
#'
#' Linear time (index-based, not `c()`-growth), and returns `""` when the closing
#' brace is missing rather than consuming the rest of the string.
#' @noRd
extract_braced <- function(rest) {
  chars <- strsplit(rest, "", fixed = TRUE)[[1]]
  depth <- 0L; start <- NA_integer_; end <- NA_integer_
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "{") {
      depth <- depth + 1L
      if (depth == 1L) start <- i + 1L
    } else if (ch == "}") {
      depth <- depth - 1L
      if (depth == 0L) {
        end <- i - 1L
        break
      }
    }
  }
  if (is.na(start) || is.na(end) || end < start) return("")
  paste(chars[start:end], collapse = "")
}

#' @noRd
parse_bib <- function(path) {
  txt <- tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                        collapse = "\n"), error = function(e) "")
  if (!nzchar(txt)) return(list())
  starts <- gregexpr("@[[:alpha:]]+[[:space:]]*\\{", txt, perl = TRUE)[[1]]
  if (starts[1L] == -1L) return(list())
  starts <- as.integer(starts)
  ends <- c(starts[-1L] - 1L, nchar(txt))
  refs <- list()
  for (i in seq_along(starts)) {
    entry <- substring(txt, starts[i], ends[i])
    type <- tolower(sub("^@", "",
                        regmatches(entry, regexpr("^@[[:alpha:]]+", entry, perl = TRUE))))
    # Skip @string, @comment, @preamble and other non-citation entry types.
    if (!length(type) || !type %in% BIB_ENTRY_TYPES) next
    km <- regmatches(entry, regexec(
      "^@[[:alpha:]]+[[:space:]]*\\{[[:space:]]*([^,[:space:]]+)", entry,
      perl = TRUE))[[1]]
    key <- if (length(km) >= 2L) km[2L] else NA_character_
    f <- bib_fields(entry)
    # BibLaTeX uses `date = {YYYY-MM-DD}` where BibTeX uses `year`.
    yr <- f[["year"]]
    if (is.null(yr) || is.na(yr)) {
      dt <- f[["date"]]
      if (!is.null(dt) && !is.na(dt)) yr <- substr(dt, 1, 4)
    }
    refs[[length(refs) + 1L]] <- as_reference(
      doi = f[["doi"]], pmid = f[["pmid"]], title = f[["title"]],
      author = f[["author"]], year = yr,
      id = na_if_empty(trimws(key)), source_file = path
    )
  }
  refs
}

## ---------------------------------------------------------------------------
## CSL-JSON
## ---------------------------------------------------------------------------

#' @noRd
parse_csljson <- function(path) {
  data <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(data)) return(list())
  items <- if (!is.null(data$items)) data$items else data
  # A single CSL object rather than an array.
  if (!is.null(items$DOI) || !is.null(items$title) || !is.null(items$id)) {
    items <- list(items)
  }
  lapply(items, function(e) {
    auths <- e$author
    author <- if (!is.null(auths)) {
      paste(vapply(auths, function(a) as_chr(a$family)[1] %||% "", character(1)),
            collapse = "; ")
    } else {
      NA
    }
    yr <- tryCatch(e[["issued"]][["date-parts"]][[1]][[1]],
                   error = function(err) NA)
    as_reference(doi = pluck1(e, "DOI"), pmid = pluck1(e, "PMID"),
                 title = pluck1(e, "title"), author = author, year = yr,
                 id = pluck1(e, "id"), source_file = path)
  })
}

## ---------------------------------------------------------------------------
## Tabular (CSV/TSV)
## ---------------------------------------------------------------------------

#' @noRd
parse_csv <- function(path) {
  sep <- if (identical(tolower(tools::file_ext(path)), "tsv")) "\t" else ","
  data <- tryCatch(
    utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(data) || !nrow(data)) return(list())
  nm <- names(data)
  doi_col <- detect_col(nm, c("doi"))
  pmid_col <- detect_col(nm, c("pmid", "pubmed_id", "pubmed"))
  title_col <- detect_col(nm, c("title"))
  author_col <- detect_col(nm, c("author", "authors"))
  year_col <- detect_col(nm, c("year", "issued", "date"))
  # A table with no recognizable columns is treated as prose to scrape.
  if (is.null(doi_col) && is.null(pmid_col) && is.null(title_col)) {
    return(parse_text(path))
  }
  getv <- function(col, i) if (!is.null(col)) data[[col]][i] else NA
  lapply(seq_len(nrow(data)), function(i) {
    as_reference(doi = getv(doi_col, i), pmid = getv(pmid_col, i),
                 title = getv(title_col, i), author = getv(author_col, i),
                 year = getv(year_col, i), source_file = path, location = i)
  })
}

## ---------------------------------------------------------------------------
## RIS
## ---------------------------------------------------------------------------

#' @noRd
parse_ris <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (!length(lines)) return(list())
  records <- list(); cur <- list(); have <- FALSE
  for (ln in lines) {
    if (grepl("^ty[[:space:]]+-", ln, ignore.case = TRUE)) {
      if (have && length(cur)) records[[length(records) + 1L]] <- list(cur)
      cur <- list(); have <- TRUE
    }
    # Tags may be uppercase, lowercase, or mixed depending on the exporter.
    m <- regmatches(ln, regexec("^([A-Za-z0-9]{2})[[:space:]]+-[[:space:]]*(.*)$", ln))[[1]]
    if (length(m) == 3L) {
      tag <- toupper(m[2L]); cur[[tag]] <- c(cur[[tag]], trimws(m[3L]))
    }
    if (grepl("^er[[:space:]]+-", ln, ignore.case = TRUE)) {
      if (length(cur)) records[[length(records) + 1L]] <- list(cur)
      cur <- list(); have <- FALSE
    }
  }
  if (length(cur)) records[[length(records) + 1L]] <- list(cur)
  lapply(records, function(rl) {
    r <- rl[[1L]]
    pmid <- NA_character_
    for (t in c("AN", "ID", "C7")) {
      if (!is.null(r[[t]]) && grepl("^[0-9]{5,9}$", r[[t]][1L])) pmid <- r[[t]][1L]
    }
    as_reference(
      doi = (r[["DO"]] %||% NA)[1L],
      pmid = pmid,
      title = (r[["TI"]] %||% r[["T1"]] %||% NA)[1L],
      author = paste(r[["AU"]] %||% r[["A1"]] %||% character(0), collapse = "; "),
      year = (r[["PY"]] %||% r[["Y1"]] %||% NA)[1L],
      source_file = path
    )
  })
}

## ---------------------------------------------------------------------------
## EndNote XML
## ---------------------------------------------------------------------------

#' @noRd
xml_text1 <- function(node, xpath) {
  n <- xml2::xml_find_first(node, xpath)
  if (inherits(n, "xml_missing")) return(NA_character_)
  na_if_empty(trimws(xml2::xml_text(n)))
}

# Namespace-agnostic element xpath. `xml2::xml_ns_strip()` does not reliably make
# `.//name` match elements of namespaced documents (all .docx, some JATS), so
# `local-name()` is used to match by element name regardless of namespace.
#' @noRd
el <- function(name) sprintf("*[local-name()='%s']", name)

#' @noRd
parse_endnote <- function(path) {
  doc <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(doc)) return(list())
  recs <- xml2::xml_find_all(doc, paste0(".//", el("record")))
  lapply(recs, function(rec) {
    authors <- xml2::xml_text(xml2::xml_find_all(rec, paste0(".//", el("author"))))
    as_reference(
      doi = xml_text1(rec, paste0(".//", el("electronic-resource-num"))),
      title = xml_text1(rec, paste0(".//", el("title"))),
      year = xml_text1(rec, paste0(".//", el("year"))),
      author = paste(trimws(authors), collapse = "; "),
      source_file = path
    )
  })
}

## ---------------------------------------------------------------------------
## JATS XML (reference list of a scholarly article)
## ---------------------------------------------------------------------------

#' @noRd
parse_jats <- function(path) {
  doc <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(doc)) return(list())
  res <- parse_jats_doc(doc, source_file = path)
  if (!length(res)) return(parse_text(path))
  res
}

#' Extract reference-list entries from a parsed JATS document.
#' @noRd
parse_jats_doc <- function(doc, source_file) {
  refs <- xml2::xml_find_all(doc, paste0(".//", el("ref-list"), "//", el("ref")))
  if (!length(refs)) refs <- xml2::xml_find_all(doc, paste0(".//", el("ref")))
  if (!length(refs)) return(list())
  lapply(refs, function(rf) {
    doi <- xml_text1(rf, paste0(".//", el("pub-id"), "[@pub-id-type='doi']"))
    if (is.na(doi)) {
      doi <- xml_text1(rf, paste0(".//", el("ext-link"), "[@ext-link-type='doi']"))
    }
    pmid <- xml_text1(rf, paste0(".//", el("pub-id"), "[@pub-id-type='pmid']"))
    # Mixed-citation references often carry the DOI/PMID only as inline text.
    if (is.na(doi) || is.na(pmid)) {
      txt <- xml2::xml_text(rf)
      if (is.na(doi)) {
        d <- extract_dois(txt)
        if (length(d)) doi <- strip_wrapping(d[[1L]]$doi)
      }
      if (is.na(pmid)) {
        m <- regmatches(txt, regexpr("(?i)pmid[[:space:]:]*[0-9]{5,9}", txt, perl = TRUE))
        if (length(m) && nzchar(m)) pmid <- gsub("[^0-9]", "", m)
      }
    }
    as_reference(
      doi = doi, pmid = pmid,
      title = xml_text1(rf, paste0(".//", el("article-title"))),
      year = xml_text1(rf, paste0(".//", el("year"))),
      source_file = source_file
    )
  })
}

## ---------------------------------------------------------------------------
## Word .docx
## ---------------------------------------------------------------------------

#' @noRd
parse_docx <- function(path) {
  tmp <- tempfile("retraction_docx")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  ok <- tryCatch(
    {
      utils::unzip(path, exdir = tmp)
      TRUE
    },
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (!ok) return(list())
  docxml <- file.path(tmp, "word", "document.xml")
  if (!file.exists(docxml)) return(list())
  doc <- tryCatch(xml2::read_xml(docxml), error = function(e) NULL)
  if (is.null(doc)) return(list())
  body <- paste(xml2::xml_text(xml2::xml_find_all(doc, paste0(".//", el("t")))),
                collapse = " ")
  # Hyperlinked DOIs live as relationship targets, not visible text.
  rels <- file.path(tmp, "word", "_rels", "document.xml.rels")
  if (file.exists(rels)) {
    body <- paste(body, paste(readLines(rels, warn = FALSE), collapse = " "))
  }
  doi_refs_from_lines(strsplit(body, "\n", fixed = TRUE)[[1]], path)
}

## ---------------------------------------------------------------------------
## PDF (optional; needs the pdftools package)
## ---------------------------------------------------------------------------

#' @noRd
parse_pdf <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_warn(c(
      "Reading PDF files requires the {.pkg pdftools} package.",
      "i" = "Install it with {.code install.packages(\"pdftools\")}."
    ))
    return(list())
  }
  txt <- tryCatch(pdftools::pdf_text(path), error = function(e) NULL)
  if (is.null(txt)) return(list())
  refs <- list()
  for (pg in seq_along(txt)) {
    lines <- strsplit(txt[pg], "\n", fixed = TRUE)[[1]]
    page_refs <- doi_refs_from_lines(lines, path, loc_prefix = paste0("p", pg))
    refs <- c(refs, page_refs)
  }
  dedup_refs(refs)
}
