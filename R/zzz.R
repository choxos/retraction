# Package load hooks.

.onLoad <- function(libname, pkgname) {
  register_builtin_backends()
  invisible()
}

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  packageStartupMessage(sprintf(
    paste0(
      "retraction %s: find retracted references in documents and ",
      "bibliographies.\n  Get started with check_file(), check_dois(), or ",
      "check_refs(). GitHub: https://github.com/choxos/retraction"
    ),
    version
  ))
  invisible()
}
