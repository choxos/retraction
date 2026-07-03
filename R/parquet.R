# Export the retraction corpus as Parquet for large-scale, out-of-memory
# analysis with arrow/dplyr. (Roadmap F29.)
#
# The in-memory snapshot with its DOI hash index already makes checking large
# reference lists fast; Parquet is for users who want to query or join the whole
# corpus with arrow themselves.

#' Export the local snapshot to a Parquet file
#'
#' Writes the current offline snapshot (built by [retraction_sync()]) to Parquet
#' so it can be queried at scale with the arrow package, e.g.
#' `arrow::open_dataset(path)`.
#'
#' @param path Output path. Defaults to `snapshot.parquet` in the cache
#'   directory ([retraction_cache_dir()]).
#' @return Invisibly, the path written. Requires the suggested arrow package.
#' @examples
#' \dontrun{
#' retraction_sync()
#' p <- retraction_snapshot_parquet()
#' arrow::open_dataset(p)
#' }
#' @export
retraction_snapshot_parquet <- function(path = NULL) {
  need_pkg("arrow", "to write a Parquet snapshot")
  snap <- load_snapshot()
  if (is.null(snap)) {
    cli::cli_abort(c("No local snapshot to export.",
                     "i" = "Run {.run retraction_sync()} first."))
  }
  if (is.null(path)) {
    path <- file.path(retraction_cache_dir(create = TRUE), "snapshot.parquet")
  }
  df <- strip_norm_columns(snap)
  attr(df, "doi_index") <- NULL
  attr(df, "synced_at") <- NULL
  arrow::write_parquet(df, path)
  cli::cli_alert_success("Wrote {nrow(df)} records -> {.file {path}}")
  invisible(path)
}
