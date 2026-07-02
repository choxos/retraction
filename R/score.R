# Status classification and match confidence scoring.

# Canonical status vocabulary and their human-readable notice labels.
STATUS_LABELS <- c(
  retracted             = "Retraction",
  expression_of_concern = "Expression of Concern",
  correction            = "Correction",
  reinstated            = "Reinstatement",
  none                  = "None",
  other                 = "Other notice",
  unchecked             = "Unchecked"
)

# Default set of notice labels that count as "flag this citation".
DEFAULT_FLAG_NATURE <- c("Retraction", "Expression of Concern")

#' Map a raw Retraction Watch `retraction_nature` (or free text) to a status.
#' @param nature Character scalar, e.g. "Retraction", "Expression of Concern".
#' @return One of the names of `STATUS_LABELS`.
#' @noRd
classify_status <- function(nature) {
  n <- tolower(na_if_empty(nature))
  if (is.na(n)) return("other")
  if (grepl("reinstat", n)) return("reinstated")
  if (grepl("expression of concern|concern", n)) return("expression_of_concern")
  if (grepl("correction|corrigend|erratum", n)) return("correction")
  if (grepl("retract|withdraw|removal|removed", n)) return("retracted")
  "other"
}

#' Human-readable label for a status.
#' @noRd
status_label <- function(status) {
  unname(STATUS_LABELS[status] %||% "Other notice")
}

#' Which statuses should be flagged, given a set of notice labels?
#' @param flag_nature Character vector of notice labels (see `DEFAULT_FLAG_NATURE`).
#' @noRd
flagged_statuses <- function(flag_nature = DEFAULT_FLAG_NATURE) {
  names(STATUS_LABELS)[STATUS_LABELS %in% flag_nature]
}

#' Is a status flagged under the given notice labels?
#' @noRd
status_is_flagged <- function(status, flag_nature = DEFAULT_FLAG_NATURE) {
  status %in% flagged_statuses(flag_nature)
}

#' Confidence that a candidate reference is the same work as a matched record.
#'
#' Exact identifier matches are near-certain; fuzzy title matches are scored in
#' a lower band and never reach the exact-match ceiling, so a fuzzy hit is
#' always reported as "possible" rather than asserted.
#'
#' @param match_type One of "doi_exact", "pmid_exact", "title_exact",
#'   "title_fuzzy".
#' @param title_sim Title similarity in `[0, 1]` (fuzzy only).
#' @param year_delta Absolute difference in publication year, or `NA`.
#' @param author_overlap Number of shared author family names, or `NA`.
#' @return A confidence in `[0, 1]`.
#' @noRd
score_match <- function(match_type, title_sim = NA_real_,
                        year_delta = NA_real_, author_overlap = NA_real_) {
  switch(
    match_type,
    doi_exact   = 1.00,
    pmid_exact  = 0.99,
    title_exact = 0.95,
    title_fuzzy = {
      s <- title_sim %||% 0
      if (is.na(s)) s <- 0
      # 0.85 similarity -> 0.50, 1.00 similarity -> 0.90.
      base <- 0.50 + 0.40 * max(0, min(1, (s - 0.85) / 0.15))
      adj <- 0
      if (!is.na(year_delta)) {
        adj <- adj + if (year_delta == 0) 0.03 else if (year_delta <= 1) 0 else -0.05
      }
      if (!is.na(author_overlap)) {
        adj <- adj + if (author_overlap > 0) 0.03 else -0.05
      }
      max(0, min(0.94, base + adj))
    },
    0.0
  )
}

#' Default confidence threshold above which a flagged match is asserted rather
#' than reported as "possible".
#' @noRd
min_confidence <- function() {
  getOption("retraction.min_confidence", 0.90)
}
