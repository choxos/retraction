# Offline snapshot: download the retraction corpus once into a user cache, then
# match against it locally. This is the private, bulk, and offline path, and it
# is what makes fuzzy title matching reliable (the live API only does substring
# search). Updates are incremental: only recent retraction-year slices are
# re-fetched and merged by record id, rather than re-downloading everything.

# In-memory cache so a batch check reads the snapshot from disk only once.
.snapshot_cache <- new.env(parent = emptyenv())

#' Location of the retraction cache directory
#'
#' @param create Create the directory if it does not exist.
#' @return The cache directory path (via [tools::R_user_dir()]).
#' @export
#' @examples
#' retraction_cache_dir()
retraction_cache_dir <- function(create = FALSE) {
  d <- tools::R_user_dir("retraction", "cache")
  if (create && !dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' @noRd
snapshot_path <- function() file.path(retraction_cache_dir(), "snapshot.rds")

#' @noRd
add_norm_columns <- function(snap) {
  n <- nrow(snap)
  snap$norm_odoi <- normalize_doi(snap$original_paper_doi %||% rep(NA_character_, n))
  snap$norm_rdoi <- normalize_doi(snap$retraction_doi %||% rep(NA_character_, n))
  snap$norm_title <- normalize_title(snap$title %||% rep(NA_character_, n))
  # PMID columns are present in snapshots exported after PMID support was added;
  # older snapshots lack them and get all-NA (offline PMID matching then no-ops).
  snap$norm_opmid <- normalize_pmid(snap$original_paper_pubmed_id %||% rep(NA_character_, n))
  snap$norm_rpmid <- normalize_pmid(snap$retraction_pubmed_id %||% rep(NA_character_, n))
  snap
}

#' @noRd
strip_norm_columns <- function(snap) {
  snap[, setdiff(names(snap), c("norm_odoi", "norm_rdoi", "norm_title",
                                "norm_opmid", "norm_rpmid")),
       drop = FALSE]
}

#' Download year-sliced export pages and bind them.
#' @return A list with `df` (or NULL) and `truncated` (logical).
#' @noRd
#' Row-bind data frames, aligning by the union of their column names.
#' @noRd
rbind_union <- function(frames) {
  frames <- Filter(function(f) !is.null(f) && nrow(f), frames)
  if (!length(frames)) return(NULL)
  cols <- Reduce(union, lapply(frames, names))
  aligned <- lapply(frames, function(f) {
    for (c in setdiff(cols, names(f))) f[[c]] <- NA_character_
    f[, cols, drop = FALSE]
  })
  do.call(rbind, aligned)
}

#' @noRd
download_slices <- function(years, quiet) {
  show_bar <- !quiet && interactive()
  if (show_bar) cli::cli_progress_bar("Downloading", total = length(years))
  frames <- list()
  for (y in years) {
    df <- xera_export_slice(year_from = y, year_to = y, limit = 10000L)
    # A year at the export cap is fetched completely via paginated /papers so no
    # records are silently truncated.
    if (!is.null(df) && nrow(df) >= 10000L) {
      full <- xera_papers_year(y)
      if (!is.null(full) && nrow(full) >= nrow(df)) df <- full
    }
    if (!is.null(df) && nrow(df)) frames[[length(frames) + 1L]] <- df
    if (show_bar) cli::cli_progress_update()
  }
  if (show_bar) cli::cli_progress_done()
  list(df = rbind_union(frames))
}

#' Download or update a local snapshot of the retraction corpus
#'
#' The first call downloads the full corpus (sliced by retraction year to
#' respect the export endpoint's row cap and rate limit). Later calls are
#' incremental by default: only recent years are re-fetched and merged into the
#' existing snapshot by record id, so new retractions are added without
#' re-downloading everything. Use `force = TRUE` for a complete refresh.
#'
#' Once a snapshot exists, pass `offline = TRUE` to any `check_*()` function to
#' match locally without the network.
#'
#' @param force Re-download the entire corpus, replacing any existing snapshot.
#' @param incremental When a snapshot exists, fetch only recent years and merge
#'   (default). Ignored when `force = TRUE` or no snapshot exists.
#' @param quiet Suppress progress messages.
#' @return Invisibly, the snapshot data frame.
#' @examples
#' \dontrun{
#' retraction_sync()                 # first run: full download
#' retraction_sync()                 # later: incremental update
#' retraction_sync(force = TRUE)     # occasional full refresh
#' check_bib("refs.bib", offline = TRUE)
#' }
#' @export
retraction_sync <- function(force = FALSE, incremental = TRUE, quiet = FALSE) {
  path <- snapshot_path()
  existing <- if (file.exists(path) && !force) load_snapshot() else NULL

  this_year <- as.integer(format(Sys.Date(), "%Y"))
  yr <- xera_year_range()
  ymin <- yr$min %||% 1970L
  ymax <- yr$max %||% this_year
  if (is.na(ymin)) ymin <- 1970L
  if (is.na(ymax)) ymax <- this_year

  retraction_cache_dir(create = TRUE)

  if (is.null(existing) || !incremental) {
    if (!quiet) cli::cli_alert_info("Downloading full retraction snapshot for {ymin}-{ymax} ...")
    dl <- download_slices(ymin:ymax, quiet)
    if (is.null(dl$df)) {
      cli::cli_abort(c(
        "Could not download any retraction records.",
        "i" = "Check your connection and {.code getOption(\"retraction.base_url\")}."
      ))
    }
    snap <- dl$df[!duplicated(dl$df$record_id), , drop = FALSE]
    n_new <- nrow(snap); n_upd <- 0L
  } else {
    # Incremental: refetch from a couple of years before the newest retraction
    # date through the current year, then upsert by record id.
    last_year <- suppressWarnings(max(as.integer(substr(existing$retraction_date, 1, 4)),
                                      na.rm = TRUE))
    if (!is.finite(last_year)) {
      synced <- attr(existing, "synced_at")
      last_year <- if (!is.null(synced)) {
        as.integer(format(as.Date(synced), "%Y"))
      } else {
        this_year - 1L
      }
    }
    lo <- max(ymin, last_year - 1L)
    hi <- max(lo, min(ymax, this_year))
    years <- lo:hi
    if (!quiet) cli::cli_alert_info("Updating snapshot: fetching retractions for {lo}-{hi} ...")
    dl <- download_slices(years, quiet)
    existing_raw <- strip_norm_columns(existing)

    if (is.null(dl$df)) {
      snap <- existing_raw; n_new <- 0L; n_upd <- 0L
    } else {
      newdf <- dl$df[!duplicated(dl$df$record_id), , drop = FALSE]
      changed <- length(setdiff(names(newdf), names(existing_raw))) > 0 ||
        length(setdiff(names(existing_raw), names(newdf))) > 0
      if (!quiet && changed) {
        cli::cli_alert_info("The API schema changed; columns are preserved via a union merge.")
      }
      merged <- tryCatch(
        rbind_union(list(
          newdf,
          existing_raw[!existing_raw$record_id %in% newdf$record_id, , drop = FALSE]
        )),
        error = function(e) NULL
      )
      if (is.null(merged)) {
        if (!quiet) cli::cli_alert_warning("Incremental merge failed; performing a full refresh.")
        return(retraction_sync(force = TRUE, incremental = FALSE, quiet = quiet))
      }
      n_new <- sum(!newdf$record_id %in% existing_raw$record_id)
      n_upd <- sum(newdf$record_id %in% existing_raw$record_id)
      snap <- merged
    }
  }

  snap <- add_norm_columns(snap)
  attr(snap, "synced_at") <- Sys.time()
  # Write atomically so an interrupted write cannot corrupt the snapshot. Only
  # advance the in-memory cache once the on-disk file is actually replaced
  # (file.rename over an existing file can fail on some platforms).
  tmp <- paste0(path, ".tmp")
  saveRDS(snap, tmp)
  if (!file.rename(tmp, path)) {
    ok <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
    if (!ok) cli::cli_abort("Could not write the snapshot to {.file {path}}.")
  }
  .snapshot_cache$data <- snap
  .snapshot_cache$freshness_checked <- TRUE

  if (!quiet) {
    cli::cli_alert_success("Snapshot ready: {nrow(snap)} records ({n_new} new, {n_upd} updated).")
  }
  invisible(snap)
}

#' Load the local snapshot, or NULL if none exists.
#' @noRd
load_snapshot <- function(reload = FALSE) {
  if (!reload && !is.null(.snapshot_cache$data)) {
    snap <- .snapshot_cache$data
  } else {
    path <- snapshot_path()
    if (!file.exists(path)) return(NULL)
    snap <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(snap) && (is.null(snap$norm_odoi) || is.null(snap$norm_opmid))) {
      snap <- add_norm_columns(snap)
    }
  }
  if (is.null(snap)) return(NULL)
  # Build the original-DOI hash index once, so the offline DOI lookup is O(1)
  # instead of an O(n) scan of the whole corpus for every reference.
  if (is.null(attr(snap, "doi_index"))) {
    snap <- attach_doi_index(snap)
    .snapshot_cache$data <- snap
  }
  snap
}

#' Attach an environment mapping normalized original DOI -> row indices.
#' @noRd
attach_doi_index <- function(snap) {
  if (is.null(snap) || !nrow(snap)) return(snap)
  by_doi <- split(seq_len(nrow(snap)), snap$norm_odoi)  # NA keys are dropped
  attr(snap, "doi_index") <- list2env(by_doi, hash = TRUE, parent = emptyenv())
  snap
}

#' Clear the package cache
#'
#' Removes the local retraction snapshot and any cached PubMed Central XML, and
#' resets the in-memory cache.
#'
#' @return Invisibly `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' retraction_clear_cache()
#' }
retraction_clear_cache <- function() {
  path <- snapshot_path()
  if (file.exists(path)) unlink(path)
  pmc_dir <- file.path(retraction_cache_dir(), "pmc")
  if (dir.exists(pmc_dir)) unlink(pmc_dir, recursive = TRUE)
  .snapshot_cache$data <- NULL
  .snapshot_cache$freshness_checked <- NULL
  invisible(TRUE)
}

#' Warn (once per session) if the offline snapshot is behind the live database.
#'
#' Opt-in only: offline mode is fully local by default so it does not leak a
#' network request. Set `options(retraction.check_freshness = TRUE)` to enable a
#' single small freshness request during offline checks.
#' @noRd
snapshot_freshness_check <- function(snap) {
  if (!isTRUE(getOption("retraction.check_freshness", FALSE))) return(invisible())
  if (isTRUE(.snapshot_cache$freshness_checked)) return(invisible())
  .snapshot_cache$freshness_checked <- TRUE
  if (is.null(snap) || !nrow(snap)) return(invisible())

  synced <- attr(snap, "synced_at")
  local_newest <- suppressWarnings(max(as.Date(snap$retraction_date), na.rm = TRUE))

  res <- tryCatch(
    xera_get("papers", list(per_page = 1, sort_by = "retraction_date",
                            sort_order = "desc")),
    error = function(e) NULL
  )

  if (is.null(res)) {
    age <- if (!is.null(synced)) as.numeric(difftime(Sys.time(), synced, units = "days")) else NA
    if (!is.na(age) && age > 30) {
      cli::cli_alert_info(paste0(
        "Your offline retraction snapshot is {round(age)} days old; run ",
        "{.code retraction_sync()} when online to update."
      ))
    }
    return(invisible())
  }

  server_total <- suppressWarnings(as.numeric(pluck1(res, "total")))
  items <- pluck1(res, "items")
  server_newest <- if (length(items)) {
    parse_api_date(pluck1(items[[1L]], "retraction_date"))
  } else {
    as.Date(NA)
  }

  outdated <- (!is.na(server_newest) && !is.na(local_newest) && server_newest > local_newest) ||
    (is.finite(server_total) && server_total > nrow(snap))

  if (outdated) {
    synced_txt <- if (!is.null(synced)) sprintf(", synced %s", format(as.Date(synced))) else ""
    cli::cli_alert_warning(c(
      "Your offline retraction snapshot is behind the live database.",
      "i" = paste0("Snapshot: {nrow(snap)} records{synced_txt}. Run ",
                   "{.code retraction_sync()} to add the latest retractions.")
    ))
  }
  invisible()
}

#' Convert a snapshot data-frame row to an API-shaped item list.
#' @noRd
snap_row_to_item <- function(row) as.list(row)

#' Match a reference against the local snapshot.
#' @noRd
snapshot_hit <- function(snap, ref, ctx) {
  if (is.null(snap) || !nrow(snap)) return(NULL)

  if (is_nonempty_string(ref$doi)) {
    index <- attr(snap, "doi_index")
    idx <- if (!is.null(index)) {
      get0(ref$doi, envir = index, inherits = FALSE) %||% integer(0)
    } else {
      which(snap$norm_odoi == ref$doi)
    }
    if (length(idx)) {
      items <- lapply(idx, function(i) snap_row_to_item(snap[i, ]))
      return(build_xera_hit(pick_governing(items), matched_on = "original_doi"))
    }
    idx <- which(snap$norm_rdoi == ref$doi)
    if (length(idx)) {
      hit <- build_xera_hit(snap_row_to_item(snap[idx[1L], ]),
                            matched_on = "retraction_doi", status_override = "none")
      hit$evidence <- "cited_retraction_notice"
      return(hit)
    }
    return(NULL)
  }

  # Offline PMID match (snapshots exported after PMID support carry the columns).
  if (is_nonempty_string(ref$pmid) && !is.null(snap$norm_opmid)) {
    idx <- which(snap$norm_opmid == ref$pmid)
    if (length(idx)) {
      items <- lapply(idx, function(i) snap_row_to_item(snap[i, ]))
      return(build_xera_hit(pick_governing(items), matched_on = "pmid",
                            match_type = "pmid_exact"))
    }
    idx <- which(snap$norm_rpmid == ref$pmid)
    if (length(idx)) {
      hit <- build_xera_hit(snap_row_to_item(snap[idx[1L], ]),
                            matched_on = "retraction_doi", match_type = "pmid_exact",
                            status_override = "none")
      hit$evidence <- "cited_retraction_notice"
      return(hit)
    }
  }

  if (isTRUE(ctx$allow_fuzzy) && is_nonempty_string(ref$title)) {
    rt <- normalize_title(ref$title)
    sims <- title_similarity(rt, snap$norm_title)
    best <- which.max(sims)
    if (length(best) && !is.na(sims[best]) && sims[best] >= fuzzy_threshold()) {
      it <- snap_row_to_item(snap[best, ])
      yd <- year_delta(ref$year, year_of(it$original_paper_date))
      ao <- author_overlap(ref$author, it$author)
      mt <- if (is_exact_metadata(sims[best], ref, it)) "title_exact" else "title_fuzzy"
      conf <- score_match(mt, title_sim = sims[best], year_delta = yd,
                          author_overlap = ao)
      return(build_xera_hit(it, matched_on = "title", match_type = mt,
                            confidence = conf, evidence = mt))
    }
  }
  NULL
}
