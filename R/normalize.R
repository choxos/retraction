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
  x <- sub("^doi:\\s*", "", x, ignore.case = TRUE)
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)
  x <- tolower(x)
  x <- sub("[.,;:\"'\\]}>]+$", "", x, perl = TRUE)
  x <- trimws(x)
  ifelse(is.na(x) | x == "" | !startsWith(x, "10."), NA_character_, x)
}

#' Normalize a PubMed identifier to bare digits.
#'
#' @param x A character (or numeric) vector of PMIDs, optionally `PMID:`-prefixed.
#' @return A character vector of digit-only PMIDs, `NA` where none is present.
#' @examples
#' normalize_pmid("PMID: 12345678")
#' @export
normalize_pmid <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^pmid:?\\s*", "", x, ignore.case = TRUE)
  x <- gsub("[^0-9]", "", x)
  ifelse(is.na(x) | x == "", NA_character_, x)
}

#' Normalize a title for fuzzy comparison only.
#'
#' Lowercases, removes a leading "Retracted:" style marker, strips markup and
#' punctuation, and collapses whitespace. The original title is retained
#' elsewhere for reporting; this form is used solely for string distance.
#'
#' @param x A character vector of titles.
#' @return A character vector of normalized titles.
#' @export
normalize_title <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  x <- sub("^\\s*(retracted(\\s+article)?|withdrawn|expression of concern)\\s*[:.-]+\\s*",
           "", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

#' Does `x` look like a DOI?
#' @noRd
looks_like_doi <- function(x) {
  grepl("10\\.[0-9]{4,9}/", x, perl = TRUE)
}

#' Does `x` look like a bare PMID (all digits, plausible length)?
#' @noRd
looks_like_pmid <- function(x) {
  grepl("^[0-9]{1,9}$", trimws(as.character(x)))
}
