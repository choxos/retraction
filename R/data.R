#' Example references for demonstrating retraction checks
#'
#' A small, stable set of references used in examples and tests. The first
#' reference (Wakefield et al. 1998) was retracted by The Lancet in 2010; the
#' other two are controls that have not been retracted. Bundled so that examples
#' can run without network access.
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{doi}{Digital Object Identifier of the reference.}
#'   \item{title}{Article title.}
#'   \item{year}{Publication year.}
#'   \item{note}{Whether the item is retracted, for reference.}
#' }
#' @source Retraction Watch via the XeraRetractionTracker API,
#'   <https://openscience.xera.ac/retractions>.
#' @examples
#' retraction_example
#' \donttest{
#' check_refs(retraction_example)
#' }
"retraction_example"
