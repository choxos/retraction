# Suggest what to cite instead of a retracted work: its correction,
# reinstatement, or the related records the corpus links to it.

#' Suggest alternatives to a retracted work
#'
#' Given a retracted work's DOI, return the records the corpus links to it, such
#' as a later correction or reinstatement, to help decide what (if anything) to
#' cite instead.
#'
#' @param doi A DOI (of the retracted work).
#' @return A [tibble][tibble::tibble] of related records (`record_id`, `title`,
#'   `nature`, `date`), or `NULL` if the DOI is unknown or has no related
#'   records.
#' @examples
#' \donttest{
#' suggest_alternatives("10.1016/S0140-6736(97)11096-0")
#' }
#' @export
suggest_alternatives <- function(doi) {
  ndoi <- normalize_doi(doi)[1]
  if (is.na(ndoi)) cli::cli_abort("{.arg doi} is not a valid DOI.")
  items <- xera_search_doi(ndoi)
  if (is.null(items) || !length(items)) return(NULL)
  # The search is a substring match, so pick the record whose original DOI is
  # exactly this DOI rather than assuming the first hit is the right one.
  match_item <- NULL
  for (it in items) {
    if (identical(normalize_doi(pluck1(it, "original_paper_doi") %||% NA_character_),
                  ndoi)) {
      match_item <- it
      break
    }
  }
  if (is.null(match_item)) return(NULL)
  rid <- na_if_empty(pluck1(match_item, "record_id"))
  if (is.na(rid)) return(NULL)
  res <- xera_get(paste0("papers/", utils::URLencode(as.character(rid)), "/related"))
  rel <- pluck1(res, "related_papers")
  if (is.null(rel) || !length(rel)) return(NULL)
  g <- function(k) {
    vapply(rel, function(r) na_if_empty(pluck1(r, k)) %||% NA_character_, character(1))
  }
  tibble::tibble(
    record_id = g("record_id"),
    title = g("title"),
    nature = g("retraction_nature"),
    date = as.Date(substr(g("retraction_date"), 1, 10))
  )
}
