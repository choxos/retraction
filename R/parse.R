# Input parsing. Each parser returns a list of normalized references (see
# `as_reference()`), tagged with their source file and, where available, a
# location (line number or page). Formats split into structured (identifiers
# preserved) and heuristic (DOIs scraped from text).

# A permissive DOI pattern; trailing prose punctuation is trimmed afterwards.
DOI_PATTERN <- "10\\.[0-9]{4,9}/[^[:space:]\"'<>]+"

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
    docx = "docx",
    pdf = "pdf",
    xml = sniff_xml(path),
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

#' Extract the value of one bib field from an entry string.
#' @noRd
bib_field <- function(entry, field) {
  pat <- sprintf("(?i)[,{][[:space:]]*%s[[:space:]]*=[[:space:]]*", field)
  m <- regexpr(pat, entry, perl = TRUE)
  if (m[1L] == -1L) return(NA_character_)
  rest <- substring(entry, m[1L] + attr(m, "match.length"))
  val <- if (startsWith(rest, "{")) {
    extract_braced(rest)
  } else if (startsWith(rest, "\"")) {
    sub("^\"([^\"]*)\".*", "\\1", rest, perl = TRUE)
  } else {
    sub("^([^,}\\n]*).*", "\\1", rest, perl = TRUE)
  }
  val <- gsub("[{}]", "", val)
  val <- gsub("[[:space:]]+", " ", val)
  na_if_empty(trimws(val))
}

#' Return the substring inside a leading `{...}`, honoring nested braces.
#' @noRd
extract_braced <- function(rest) {
  chars <- strsplit(rest, "", fixed = TRUE)[[1]]
  depth <- 0L; out <- character(0)
  for (ch in chars) {
    if (ch == "{") { depth <- depth + 1L; if (depth == 1L) next }
    if (ch == "}") { depth <- depth - 1L; if (depth == 0L) break }
    out <- c(out, ch)
  }
  paste(out, collapse = "")
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
    km <- regmatches(entry, regexec(
      "^@[[:alpha:]]+[[:space:]]*\\{[[:space:]]*([^,[:space:]]+)", entry,
      perl = TRUE))[[1]]
    key <- if (length(km) >= 2L) km[2L] else NA_character_
    refs[[i]] <- as_reference(
      doi = bib_field(entry, "doi"),
      pmid = bib_field(entry, "pmid"),
      title = bib_field(entry, "title"),
      author = bib_field(entry, "author"),
      year = bib_field(entry, "year"),
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
    } else NA
    yr <- tryCatch(e[["issued"]][["date-parts"]][[1]][[1]],
                   error = function(err) NA)
    as_reference(doi = pluck1(e, "DOI"), pmid = pluck1(e, "PMID"),
                 title = pluck1(e, "title"), author = author, year = yr,
                 id = pluck1(e, "id"), source_file = path)
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
    if (grepl("^TY[[:space:]]+-", ln)) {
      if (have && length(cur)) records[[length(records) + 1L]] <- list(cur)
      cur <- list(); have <- TRUE
    }
    m <- regmatches(ln, regexec("^([A-Z0-9]{2})[[:space:]]+-[[:space:]]*(.*)$", ln))[[1]]
    if (length(m) == 3L) {
      tag <- m[2L]; cur[[tag]] <- c(cur[[tag]], trimws(m[3L]))
    }
    if (grepl("^ER[[:space:]]+-", ln)) {
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

#' @noRd
parse_endnote <- function(path) {
  doc <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(doc)) return(list())
  xml2::xml_ns_strip(doc)
  recs <- xml2::xml_find_all(doc, ".//record")
  lapply(recs, function(rec) {
    authors <- xml2::xml_text(xml2::xml_find_all(rec, ".//contributors/authors/author"))
    as_reference(
      doi = xml_text1(rec, ".//electronic-resource-num"),
      title = xml_text1(rec, ".//titles/title"),
      year = xml_text1(rec, ".//dates/year"),
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
  xml2::xml_ns_strip(doc)
  refs <- xml2::xml_find_all(doc, ".//ref-list//ref")
  if (!length(refs)) refs <- xml2::xml_find_all(doc, ".//ref")
  if (!length(refs)) return(parse_text(path))
  out <- lapply(refs, function(rf) {
    as_reference(
      doi = xml_text1(rf, ".//pub-id[@pub-id-type='doi']"),
      pmid = xml_text1(rf, ".//pub-id[@pub-id-type='pmid']"),
      title = xml_text1(rf, ".//article-title"),
      year = xml_text1(rf, ".//year"),
      source_file = path
    )
  })
  out
}

## ---------------------------------------------------------------------------
## Word .docx
## ---------------------------------------------------------------------------

#' @noRd
parse_docx <- function(path) {
  tmp <- tempfile("retraction_docx")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  ok <- tryCatch({ utils::unzip(path, exdir = tmp); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) return(list())
  docxml <- file.path(tmp, "word", "document.xml")
  if (!file.exists(docxml)) return(list())
  doc <- tryCatch(xml2::read_xml(docxml), error = function(e) NULL)
  if (is.null(doc)) return(list())
  xml2::xml_ns_strip(doc)
  body <- paste(xml2::xml_text(xml2::xml_find_all(doc, ".//t")), collapse = "")
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
