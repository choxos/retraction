# Preprint withdrawal detection for arXiv and bioRxiv. A withdrawn preprint
# should not be cited as if live, so it is treated like a retraction.

#' Classify an identifier as an arXiv or bioRxiv preprint.
#'
#' @return A list with `server` ("arxiv"/"biorxiv") and `id`, or `NULL`.
#' @noRd
preprint_ref <- function(doi = NA, id = NA) {
  doi <- na_if_empty(doi); id <- na_if_empty(id)
  if (!is.na(doi)) {
    d <- tolower(doi)
    if (grepl("^10\\.48550/arxiv\\.", d)) {
      return(list(server = "arxiv", id = sub("(?i)^10\\.48550/arxiv\\.", "", doi, perl = TRUE)))
    }
    if (grepl("^10\\.1101/", d)) return(list(server = "biorxiv", id = doi))
  }
  if (!is.na(id)) {
    if (grepl("(?i)^arxiv:", id)) return(list(server = "arxiv", id = sub("(?i)^arxiv:", "", id)))
    if (grepl("^[0-9]{4}\\.[0-9]{4,5}(v[0-9]+)?$", id)) return(list(server = "arxiv", id = id))
  }
  NULL
}

#' Does free text signal a withdrawal? (pure; unit-testable)
#' @noRd
is_withdrawn_text <- function(...) {
  txt <- tolower(paste(stats::na.omit(c(...)), collapse = " "))
  grepl("withdrawn|has been withdrawn|withdrawal", txt)
}

#' Query arXiv for withdrawal. NULL on failure.
#' @noRd
arxiv_withdrawn <- function(id) {
  # HTTPS, through the shared perform layer (timeout, retry, user-agent), rather
  # than xml2::read_xml() fetching a bare http:// URL with no timeout.
  txt <- http_get_text("https://export.arxiv.org/api/query", list(id_list = id))
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  doc <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  el <- function(n) {
    na_if_empty(xml2::xml_text(
      xml2::xml_find_first(doc, paste0(".//*[local-name()='", n, "']"))))
  }
  list(withdrawn = is_withdrawn_text(el("summary"), el("comment"), el("title")),
       title = el("title"))
}

#' Query bioRxiv for withdrawal. NULL on failure.
#' @noRd
biorxiv_withdrawn <- function(doi) {
  # The DOI sits in the path with its slash intact, so it is not reserved-encoded.
  res <- http_get_json(paste0("https://api.biorxiv.org/details/biorxiv/",
                              utils::URLencode(doi)))
  if (is.null(res)) return(NULL)
  coll <- pluck1(res, "collection") %||% list()
  if (!length(coll)) return(list(withdrawn = FALSE, title = NA_character_))
  last <- coll[[length(coll)]]
  list(
    withdrawn = is_withdrawn_text(pluck1(last, "type"), pluck1(last, "abstract")),
    title = na_if_empty(pluck1(last, "title"))
  )
}

#' Check whether a preprint has been withdrawn
#'
#' Accepts an arXiv identifier (e.g. `"2401.01234"` or `"arXiv:2401.01234"`), an
#' arXiv DOI (`10.48550/arXiv.*`), or a bioRxiv DOI (`10.1101/*`).
#'
#' @param x A preprint identifier or DOI.
#' @return A one-row [tibble][tibble::tibble] with `id`, `server`, `withdrawn`,
#'   and `title`; or `NULL` if `x` is not a recognized preprint identifier.
#' @examples
#' \donttest{
#' check_preprint("10.1101/2020.01.30.927871")
#' }
#' @export
check_preprint <- function(x) {
  x <- as_chr(x)[1]
  pp <- preprint_ref(doi = x, id = x)
  if (is.null(pp)) {
    cli::cli_warn("{.arg x} is not a recognized arXiv or bioRxiv identifier.")
    return(NULL)
  }
  info <- if (pp$server == "arxiv") arxiv_withdrawn(pp$id) else biorxiv_withdrawn(pp$id)
  if (is.null(info)) {
    cli::cli_warn("Could not reach the {pp$server} API.")
    return(NULL)
  }
  tibble::tibble(id = pp$id, server = pp$server,
                 withdrawn = isTRUE(info$withdrawn), title = info$title)
}

#' @noRd
backend_preprint <- function(ref, ctx) {
  if (isTRUE(ctx$offline)) return(new_hit("preprint", 7L))
  pp <- preprint_ref(doi = ref$doi, id = ref$query %||% ref$id)
  if (is.null(pp)) return(new_hit("preprint", 7L))
  info <- if (pp$server == "arxiv") arxiv_withdrawn(pp$id) else biorxiv_withdrawn(pp$id)
  if (is.null(info)) return(new_hit("preprint", 7L, state = "failed"))
  if (!isTRUE(info$withdrawn)) {
    return(new_hit("preprint", 7L, checked = TRUE, title = info$title))
  }
  new_hit("preprint", 7L, checked = TRUE, matched = TRUE, status = "retracted",
          doi = ref$doi %||% NA_character_, title = info$title,
          nature = "Withdrawal", notice_type = "Withdrawal",
          status_source = pp$server, matched_on = "doi", match_type = "doi_exact",
          confidence = score_match("doi_exact"), evidence = "preprint_withdrawn")
}
