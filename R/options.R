# Configuration accessors. Precedence is always: explicit option, then
# environment variable, then a built-in default.

# Default XeraRetractionTracker API base (serves the Retraction Watch database).
DEFAULT_BASE_URL <- "https://openscience.xera.ac/retractions/api/v1"

#' Resolve the Xera API base URL.
#' @noRd
retraction_base_url <- function() {
  url <- getOption("retraction.base_url")
  if (!is_nonempty_string(url)) url <- Sys.getenv("RETRACTION_BASE_URL", "")
  if (!nzchar(url)) url <- DEFAULT_BASE_URL
  sub("/+$", "", url)
}

#' Resolve the polite-pool contact email (used by Crossref and OpenAlex).
#' Returns `NULL` when unset.
#' @noRd
retraction_mailto <- function() {
  m <- getOption("retraction.mailto")
  if (!is_nonempty_string(m)) m <- Sys.getenv("RETRACTION_MAILTO", "")
  if (nzchar(m)) m else NULL
}

#' Default source set, from `getOption("retraction.sources")` or `"xera"`.
#' @noRd
retraction_sources_default <- function() {
  s <- as_chr(getOption("retraction.sources", "xera"))
  if (length(s)) s else "xera"
}

#' User-agent string identifying the package and repository.
#' @noRd
retraction_user_agent <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("retraction")),
                  error = function(e) "0.0.0")
  sprintf("retraction R package (%s; https://github.com/choxos/retraction)", ver)
}
