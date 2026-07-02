# Internal utilities shared across the package.

#' Null/empty coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Is `x` a non-empty, non-NA scalar string?
#' @noRd
is_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' Coerce a value to a character vector, dropping NULLs, NAs, and empties.
#' @noRd
as_chr <- function(x) {
  if (is.null(x)) return(character(0))
  x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x[!is.na(x) & nzchar(x)]
}

#' First non-empty string among the arguments, else `NA_character_`.
#' @noRd
first_nonempty <- function(...) {
  x <- as_chr(list(...))
  if (length(x)) x[[1L]] else NA_character_
}

#' Drop `NULL` elements of a list.
#' @noRd
compact <- function(x) {
  if (is.null(x) || length(x) == 0L) return(x)
  x[!vapply(x, is.null, logical(1))]
}

#' Safe nested list accessor: `pluck1(x, "a", "b")` returns `x[["a"]][["b"]]`.
#' @noRd
pluck1 <- function(x, ...) {
  keys <- c(...)
  for (k in keys) {
    if (is.null(x)) return(NULL)
    x <- tryCatch(x[[k]], error = function(e) NULL)
  }
  x
}

#' Return `x` unless it is `NULL`/empty/`NA`, in which case `NA_character_`.
#' @noRd
na_if_empty <- function(x) {
  x <- x %||% NA_character_
  if (length(x) != 1L) x <- as_chr(x)[1L] %||% NA_character_
  if (is.null(x) || is.na(x) || !nzchar(x)) NA_character_ else as.character(x)
}

#' Empty string for `NULL`/`NA`, else `x` as character (for report cells).
#' @noRd
blank_na <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) "" else as.character(x)
}
