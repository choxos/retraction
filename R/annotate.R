# Write an input bibliography back out with retracted entries annotated, so the
# warning travels with the library. (Roadmap D17.)
#
# Idempotent: an entry already carrying a "RETRACTED" note is left alone, so
# re-running does not duplicate notes. The
# inserted field changes field order (it goes right after the key); a `.bib`
# formatter may renormalize it. Assumes result `id`s are the BibTeX keys (true
# for check_file() on a .bib). BibLaTeX macros/@string are not expanded.

#' Find the line span of the entry whose `@type{` begins at `start`.
#' @noRd
bib_entry_span <- function(lines, start) {
  nxt <- grep("^\\s*@[[:alpha:]]+\\s*\\{", lines, perl = TRUE)
  end <- nxt[nxt > start]
  if (length(end)) end[1] - 1L else length(lines)
}

#' Annotate a BibTeX/BibLaTeX file, marking retracted entries.
#'
#' Inserts `note = {RETRACTED: <reason>}` into every flagged entry that does not
#' already carry a RETRACTED note.
#'
#' @param bib_path Path to the source `.bib` file.
#' @param x A `retraction_result` from checking that file (its `id` values must
#'   be the BibTeX keys).
#' @param out_path Where to write the annotated file. Defaults to
#'   `"<name>-annotated.bib"` next to the source.
#' @return Invisibly, `out_path`.
#' @examples
#' \dontrun{
#' res <- check_file("refs.bib")
#' annotate_bib("refs.bib", res)
#' }
#' @export
annotate_bib <- function(bib_path, x, out_path = NULL) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  lines <- readLines(bib_path, warn = FALSE, encoding = "UTF-8")
  if (is.null(out_path)) {
    out_path <- file.path(dirname(bib_path),
                          paste0(tools::file_path_sans_ext(basename(bib_path)),
                                 "-annotated.bib"))
  }
  fl <- retracted(x, "flagged")
  reasons <- stats::setNames(fl$reason, fl$id)
  keys <- fl$id[!is.na(fl$id)]

  hits <- lapply(keys, function(k) {
    # Escape regex metacharacters in the key (PCRE); matches "@type{key,".
    kq <- gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", k, perl = TRUE)
    at <- grep(sprintf("^\\s*@[[:alpha:]]+\\s*\\{\\s*%s\\s*,", kq), lines, perl = TRUE)
    if (!length(at)) return(NULL)
    span_end <- bib_entry_span(lines, at[1])
    already <- any(grepl("note\\s*=.*RETRACTED", lines[at[1]:span_end], perl = TRUE))
    if (already) return(NULL)
    list(at = at[1], key = k)
  })
  hits <- Filter(Negate(is.null), hits)

  # Insert from the bottom up so earlier indices stay valid.
  for (h in hits[order(-vapply(hits, `[[`, numeric(1), "at"))]) {
    reason <- na_if_empty(reasons[[h$key]]) %||% "reason unavailable"
    reason <- gsub("[{}]", "", reason)
    lines <- append(lines, sprintf("  note = {RETRACTED: %s},", reason), after = h$at)
  }
  writeLines(lines, out_path, useBytes = TRUE)
  cli::cli_alert_success("Annotated {length(hits)} entr{?y/ies} -> {.file {out_path}}")
  invisible(out_path)
}
