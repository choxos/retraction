# Author- and journal-level retraction queries against the Xera corpus.
# (Roadmap E23, E24.)
#
# Uses `rbind_union()` (R/sync.R) rather than bare `rbind` so a schema change
# across pages does not error (future-work review F4), and warns on truncation
# rather than silently capping (P6). `search/advanced` items share the paper
# shape used by `/papers`, so `papers_items_to_df()` maps them; if the endpoint
# diverges, adjust that mapper.

#' Page through a Xera `search/advanced` query into a data frame.
#'
#' @param query Named list of query parameters.
#' @param per_page,max_pages Pagination controls. Warns if `max_pages` is hit
#'   with more pages available (results truncated).
#' @noRd
xera_search_all <- function(query, per_page = 100L, max_pages = 200L) {
  page <- 1L; frames <- list(); more <- FALSE
  repeat {
    res <- xera_get("search/advanced", c(query, list(per_page = per_page, page = page)))
    items <- pluck1(res, "items") %||% list()
    n <- length(items)
    if (!n) break
    frames[[length(frames) + 1L]] <- papers_items_to_df(items)
    total_pages <- suppressWarnings(as.integer(pluck1(res, "total_pages") %||% NA))
    # When total_pages is reported, use it; otherwise infer "more" from a full page.
    more <- if (!is.na(total_pages)) page < total_pages else n >= per_page
    if (!more || page >= max_pages) break
    page <- page + 1L
  }
  if (isTRUE(more) && page >= max_pages) {
    cli::cli_alert_warning(paste0(
      "Results truncated at {max_pages * per_page} rows; ",
      "more may exist. Raise {.arg max_pages}."
    ))
  }
  if (length(frames)) rbind_union(frames) else NULL
}

#' Retractions attributed to an author.
#'
#' @param name Author name to search (family name is usually enough).
#' @param max_pages Pagination cap; raise it to fetch more than
#'   `max_pages * 100` records.
#' @return A data frame of matching retraction records, or `NULL` if none.
#' @examples
#' \donttest{
#' author_retractions("Wakefield")
#' }
#' @export
author_retractions <- function(name, max_pages = 200L) {
  if (!is_nonempty_string(name)) cli::cli_abort("{.arg name} must be a non-empty string.")
  xera_search_all(list(author = name), max_pages = max_pages)
}

#' Retraction summary for a journal.
#'
#' @param journal Journal name to search.
#' @param max_pages Pagination cap; journals can have thousands of retractions,
#'   so raise this if you need the complete set.
#' @return A list: `n`, `by_reason`, `by_year`, `records`. `NULL` if none found.
#' @examples
#' \donttest{
#' journal_retractions("The Lancet")
#' }
#' @export
journal_retractions <- function(journal, max_pages = 200L) {
  if (!is_nonempty_string(journal)) cli::cli_abort("{.arg journal} must be a non-empty string.")
  df <- xera_search_all(list(journal = journal), max_pages = max_pages)
  if (is.null(df) || !nrow(df)) return(NULL)
  list(
    n = nrow(df),
    by_reason = sort(table(stats::na.omit(df$reason)), decreasing = TRUE),
    by_year = table(substr(df$retraction_date, 1, 4)),
    records = df
  )
}
