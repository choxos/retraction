# HTTP helpers built on httr2. Every request is non-throwing: transport errors
# and non-2xx responses return NULL, so callers treat "not found" and
# "unreachable" uniformly and a single bad lookup never aborts a batch check.
# Retries cover transient 429/5xx; a token-bucket throttle keeps requests polite.

#' Perform a prepared request and parse the body, or return NULL.
#' @param req An httr2 request.
#' @param parse "json" (returns a list) or "text" (returns a string).
#' @param throttle A list with `capacity` and `fill_time_s` token-bucket
#'   settings. Defaults to roughly 5 requests/second.
#' @noRd
.http_perform <- function(req, parse = c("json", "text"),
                          throttle = list(capacity = 5, fill_time_s = 1)) {
  parse <- match.arg(parse)
  req <- httr2::req_user_agent(req, retraction_user_agent())
  req <- httr2::req_timeout(req, getOption("retraction.timeout", 30))
  req <- httr2::req_retry(
    req,
    max_tries = getOption("retraction.max_tries", 3L),
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }
  )
  req <- httr2::req_throttle(req, capacity = throttle$capacity,
                            fill_time_s = throttle$fill_time_s)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) return(NULL)

  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
  if (is.null(body) || !nzchar(body)) return(NULL)

  if (parse == "text") return(body)
  tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE),
           error = function(e) NULL)
}

#' GET a path on the configured Xera API base URL.
#' @param path Path appended to the base URL, e.g. "search/advanced".
#' @param query Named list of query parameters (NULLs dropped).
#' @param parse "json" or "text".
#' @noRd
xera_get <- function(path, query = list(), parse = "json",
                     throttle = list(capacity = 5, fill_time_s = 1)) {
  req <- httr2::request(retraction_base_url())
  req <- httr2::req_url_path_append(req, path)
  query <- compact(query)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  .http_perform(req, parse = parse, throttle = throttle)
}

#' GET an absolute URL (used by the Crossref and OpenAlex backends).
#' @noRd
http_get_json <- function(url, query = list(), headers = list()) {
  req <- httr2::request(url)
  query <- compact(query)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  headers <- compact(headers)
  if (length(headers)) req <- do.call(httr2::req_headers, c(list(req), headers))
  .http_perform(req, parse = "json")
}

#' GET an absolute URL as text, through the shared perform layer (timeout,
#' retry, user-agent). Used for XML/Atom endpoints such as arXiv.
#' @noRd
http_get_text <- function(url, query = list(), headers = list()) {
  req <- httr2::request(url)
  query <- compact(query)
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  headers <- compact(headers)
  if (length(headers)) req <- do.call(httr2::req_headers, c(list(req), headers))
  .http_perform(req, parse = "text")
}
