# Identifier and title normalization. Matching is only as good as its
# normalization, so this is deliberately conservative and well tested.

#' Normalize a DOI.
#'
#' Strips resolver prefixes (`https://doi.org/`, `doi:`), lowercases, and trims
#' trailing punctuation, following Crossref's canonicalization guidance. Empty
#' results become `NA`.
#'
#' @param x A character vector of DOIs.
#' @return A character vector of normalized DOIs, with `NA` for unparseable input.
#' @examples
#' normalize_doi("https://doi.org/10.1234/ABC. ")
#' normalize_doi("doi:10.1016/S0140-6736(97)11096-0")
#' @export
normalize_doi <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^[<\\[({]+\\s*", "", x, perl = TRUE)  # leading wrappers, e.g. <10.x>
  x <- sub("^doi:\\s*", "", x, ignore.case = TRUE)
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)
  x <- sub("^[<\\[({]+\\s*", "", x, perl = TRUE)  # wrappers left after a prefix
  x <- tolower(x)
  x <- sub("[.,;:\"'\\]}>]+$", "", x, perl = TRUE)
  # Strip an unbalanced trailing bracket left by a wrapper (e.g. "(10.x)"),
  # while preserving balanced parentheses that are part of the DOI.
  fix <- !is.na(x) & grepl("[)\\]}]$", x, perl = TRUE)
  if (any(fix)) x[fix] <- strip_wrapping(x[fix])
  x <- trimws(x)
  ifelse(is.na(x) | x == "" | !startsWith(x, "10."), NA_character_, x)
}

#' Normalize a PubMed identifier to bare digits.
#'
#' Accepts an optional `PMID:` prefix. Input that is not a bare run of digits
#' after the prefix is rejected as `NA` rather than coerced, so a PMCID or DOI
#' placed in a PMID field is not silently turned into a fabricated PMID.
#'
#' @param x A character (or numeric) vector of PMIDs, optionally `PMID:`-prefixed.
#' @return A character vector of digit-only PMIDs, `NA` where none is present.
#' @examples
#' normalize_pmid("PMID: 12345678")
#' normalize_pmid("PMC12345")  # NA: a PMCID is not a PMID
#' @export
normalize_pmid <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^pmid:?\\s*", "", x, ignore.case = TRUE)
  x <- trimws(x)
  ifelse(is.na(x) | !grepl("^[0-9]{1,9}$", x), NA_character_, x)
}

#' Normalize a title for fuzzy comparison only.
#'
#' Transliterates to lowercase ASCII Latin (so diacritics, ligatures, and
#' full-width forms fold to a comparable base form), removes a leading
#' "Retracted:" style marker, strips markup and punctuation, and collapses
#' whitespace. The original title is retained elsewhere for reporting; this form
#' is used solely for string distance. If the optional stringi package is
#' installed, non-Latin scripts (such as CJK) are also romanized; otherwise they
#' are dropped by the base fallback.
#'
#' @param x A character vector of titles.
#' @return A character vector of normalized titles.
#' @examples
#' normalize_title("Résumé of a Study")   # accents folded
#' @export
normalize_title <- function(x) {
  x <- as.character(x)
  # Unicode fold: romanize non-Latin scripts, strip accents, drop to lowercase.
  # stringi (optional) gives the fullest fold, including romanizing CJK; without
  # it, base iconv still folds accented Latin to ASCII (non-Latin scripts drop).
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_nfkc(x)
    x <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII; Lower")
  } else {
    x <- tolower(enc2utf8(x))
    x <- suppressWarnings(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
  }
  x <- sub(paste0("^\\s*[\\[(]?\\s*(retracted(\\s+article)?|retraction(\\s+of)?|",
                  "withdrawn|withdrawal|expression of concern)\\b"),
           "", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("[^[:alnum:][:space:]]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

#' Does `x` look like a DOI (registrant plus a non-empty suffix)?
#' @noRd
looks_like_doi <- function(x) {
  grepl("10\\.[0-9]{4,9}/[^[:space:]]", x, perl = TRUE)
}

#' Does `x` look like a bare PMID (all digits, plausible length)?
#' @noRd
looks_like_pmid <- function(x) {
  grepl("^[0-9]{1,9}$", trimws(as.character(x)))
}
