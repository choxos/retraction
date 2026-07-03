# Monitor a bibliography over time: save a checked baseline, then later report
# references that have become retracted since. (Roadmap C15.)
#
# Persistence is a versioned schema keyed on normalized identifiers, not the raw
# display `id`: `id` is query- or title-derived and not stable across
# re-orderings or normalization drift, which would cause false negatives (missed
# newly-retracted) and false positives.

# Bump when the baseline schema changes; a mismatched baseline is refused.
WATCH_SCHEMA_VERSION <- 1L

#' Stable per-row key: normalized DOI, else PMID, else the normalized title.
#'
#' For a title-only reference the id is title-derived; normalizing it keeps the
#' key stable across minor title-formatting differences between runs.
#' @noRd
watch_key <- function(res) {
  ifelse(!is.na(res$doi), paste0("doi:", res$doi),
         ifelse(!is.na(res$pmid), paste0("pmid:", res$pmid),
                paste0("title:", normalize_title(res$id))))
}

#' Path to a named watch file under the package cache.
#' @noRd
watch_path <- function(name) {
  if (!grepl("^[A-Za-z0-9._-]+$", name) || name %in% c("snapshot", "pmc")) {
    cli::cli_abort("{.arg name} must be a simple identifier and not a reserved name.")
  }
  file.path(retraction_cache_dir(create = TRUE), paste0("watch-", name, ".rds"))
}

#' Save a checked result as a named watch baseline.
#'
#' @param x A `retraction_result`.
#' @param name A short identifier for this bibliography (e.g. `"my-review"`).
#' @return Invisibly, the path written.
#' @export
retraction_watch_save <- function(x, name) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result}.")
  }
  baseline <- list(
    schema_version = WATCH_SCHEMA_VERSION,
    package_version = as.character(utils::packageVersion("retraction")),
    saved_at = Sys.time(),
    key = watch_key(x),
    is_retracted = x$is_retracted %in% TRUE,
    status = x$status
  )
  path <- watch_path(name)
  saveRDS(baseline, path)
  invisible(path)
}

#' Report references newly flagged since the saved baseline.
#'
#' Re-checks the same bibliography and returns rows that are flagged now but were
#' not at save time, matched by normalized identifier.
#'
#' @param x A freshly checked `retraction_result` for the same bibliography.
#' @param name The identifier used when saving the baseline.
#' @return A `retraction_result` with only the newly-flagged rows (zero rows if
#'   nothing changed).
#' @export
retraction_watch_diff <- function(x, name) {
  path <- watch_path(name)
  if (!file.exists(path)) {
    cli::cli_abort(c("No saved baseline named {.val {name}}.",
                     "i" = "Call {.code retraction_watch_save(x, \"{name}\")} first."))
  }
  prev <- readRDS(path)
  if (!identical(prev$schema_version, WATCH_SCHEMA_VERSION)) {
    cli::cli_abort(c("Saved baseline {.val {name}} uses an older schema.",
                     "i" = "Re-save it with {.code retraction_watch_save()}."))
  }
  key_now <- watch_key(x)
  prev_flagged <- stats::setNames(prev$is_retracted, prev$key)
  was_flagged <- prev_flagged[match(key_now, names(prev_flagged))]
  was_flagged[is.na(was_flagged)] <- FALSE
  newly <- (x$is_retracted %in% TRUE) & !was_flagged
  out <- x[newly, , drop = FALSE]
  if (!inherits(out, "retraction_result")) class(out) <- c("retraction_result", class(out))
  out
}
