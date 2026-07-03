# Command-line entrypoint: scan files and exit non-zero per a fail policy, for
# CI or a pre-commit hook. (Roadmap A3/A4.)
#
# Fails CLOSED: a missing file or a parse/network error is an error, not a silent
# skip. A CI gate that passes on a typo'd path is a false negative, which for a
# retraction check is the worst failure mode. Uses fail_policy() from policy.R.

#' Scan files for retracted citations.
#'
#' The testable core of the CLI (no `quit()`). Missing files error by default.
#'
#' @param files Character vector of file paths.
#' @param on_missing `"error"` (default) or `"skip"` (record as a file error).
#' @param ... Passed to [check_file()].
#' @return A list: `result` (a `retraction_result`), `n_errors` (files that could
#'   not be scanned), and `errors` (a named list of error messages).
#' @export
retraction_scan <- function(files, on_missing = c("error", "skip"), ...) {
  on_missing <- match.arg(on_missing)
  files <- as.character(files)
  missing <- files[!file.exists(files)]
  if (length(missing) && on_missing == "error") {
    cli::cli_abort(c("Cannot scan; {length(missing)} file{?s} not found:",
                     stats::setNames(missing, rep("x", length(missing)))))
  }

  errors <- list()
  results <- list()
  for (f in files) {
    if (!file.exists(f)) {
      errors[[f]] <- "file not found"
      next
    }
    r <- tryCatch(check_file(f, progress = FALSE, ...),
                  error = function(e) conditionMessage(e))
    if (inherits_result <- inherits(r, "retraction_result")) {
      results[[length(results) + 1L]] <- r
    } else {
      errors[[f]] <- r  # the error message
    }
  }

  result <- if (length(results)) bind_results(results) else new_retraction_result(list())
  list(result = result, n_errors = length(errors), errors = errors)
}

#' CLI main: scan files and exit per a fail policy.
#'
#' For `Rscript -e 'retraction::retraction_main()' [--fail-on=flagged,unchecked] file ...`.
#' Prints per-state counts and any file errors, then exits: 0 = policy satisfied,
#' 1 = policy triggered, 2 = usage/argument error.
#'
#' @param args Character vector of arguments (default: command-line trailing args).
#' @return Does not return; exits the R session.
#' @export
retraction_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  on <- "flagged"
  fo <- grep("^--fail-on=", args, value = TRUE)
  if (length(fo)) {
    on <- strsplit(sub("^--fail-on=", "", fo[1]), ",", fixed = TRUE)[[1]]
    args <- args[!grepl("^--fail-on=", args)]
  }
  if (!length(args)) {
    message("usage: retraction [--fail-on=flagged,possible,unchecked,error] <file> ...")
    quit(save = "no", status = 2L)
  }
  policy <- tryCatch(fail_policy(on), error = function(e) {
    message("retraction: ", conditionMessage(e)); quit(save = "no", status = 2L)
  })
  scan <- tryCatch(retraction_scan(args), error = function(e) {
    message("retraction: ", conditionMessage(e)); quit(save = "no", status = 2L)
  })
  print(scan$result)
  verdict <- evaluate_policy(scan$result, policy, scan$n_errors)
  print_state_counts(verdict$counts, scan$n_errors)
  if (scan$n_errors) {
    for (f in names(scan$errors)) message(sprintf("error: %s: %s", f, scan$errors[[f]]))
  }
  quit(save = "no", status = if (verdict$fail) 1L else 0L)
}
