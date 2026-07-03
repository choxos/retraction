# Report rendering. HTML is fully self-contained (inline CSS, no rmarkdown or
# pandoc dependency) so it works offline and anywhere.

#' @noRd
html_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' Distinct sources that confirmed a match, for the report footer.
#' @noRd
sources_used <- function(x) {
  s <- x$sources[!is.na(x$sources)]
  used <- unique(unlist(strsplit(s, ", ", fixed = TRUE)))
  if (length(used)) paste(used, collapse = ", ") else "the configured sources"
}

#' The reassurance line shown when nothing is flagged, honoring empty and
#' unchecked results.
#' @noRd
clean_message <- function(cnt) {
  if (cnt$n == 0L) return("No references were found in the input.")
  checked <- cnt$n - cnt$unchecked
  if (cnt$unchecked > 0L) {
    return(sprintf(
      paste0("No retracted references among the %d reference%s that were ",
             "checked; %d could not be checked."),
      checked, if (checked == 1L) "" else "s", cnt$unchecked))
  }
  sprintf("No retracted references were detected among %d reference%s.",
          cnt$n, if (cnt$n == 1L) "" else "s")
}

#' @noRd
ref_cell <- function(r) {
  loc <- if (!is.na(r$source_file)) {
    sprintf("<div class='loc'>%s%s</div>", html_escape(basename(r$source_file)),
            if (!is.na(r$location)) paste0(":", html_escape(r$location)) else "")
  } else {
    ""
  }
  if (!is.na(r$doi)) {
    sprintf("<a href='https://doi.org/%s'>%s</a>%s",
            html_escape(r$doi), html_escape(r$doi), loc)
  } else {
    paste0(html_escape(r$id), loc)
  }
}

#' @noRd
html_section <- function(heading, df, css_class = "") {
  rows <- vapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    when <- if (!is.na(r$retraction_date)) html_escape(format(r$retraction_date)) else ""
    days <- if (!is.na(r$days_since_retraction)) as.character(r$days_since_retraction) else ""
    title <- if (!is.na(r$matched_title)) html_escape(r$matched_title) else "<em>(no title)</em>"
    sprintf(
      paste0("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class='num'>%s</td>",
             "<td>%s</td><td>%s</td><td>%s</td></tr>"),
      ref_cell(r), title, html_escape(r$journal %||% NA), when, days,
      html_escape(r$reason %||% NA),
      html_escape(sub("_", " ", r$matched_on %||% NA)),
      html_escape(r$sources %||% NA)
    )
  }, character(1))
  sprintf(
    paste0("<section class='%s'><h2>%s <span class='count'>%d</span></h2>",
           "<div class='scroll'><table><thead><tr><th>Reference</th><th>Title</th>",
           "<th>Journal</th><th>Retracted</th><th>Days ago</th><th>Reason</th>",
           "<th>Matched on</th><th>Sources</th></tr></thead><tbody>%s</tbody>",
           "</table></div></section>"),
    css_class, html_escape(heading), nrow(df), paste(rows, collapse = "")
  )
}

REPORT_CSS <- paste(
  "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;",
  "margin:0;padding:2rem;color:#1a1a2e;background:#f7f7fb;line-height:1.5}",
  "h1{margin:0 0 .25rem}.meta{color:#666;margin:0 0 1.5rem}",
  ".cards{display:flex;gap:.75rem;flex-wrap:wrap;margin-bottom:1.5rem}",
  ".card{border-radius:.5rem;padding:.75rem 1rem;min-width:5rem;background:#fff;",
  "border:1px solid #e4e4ef}.card .v{font-size:1.6rem;font-weight:700}",
  ".card .l{font-size:.8rem;color:#666}.card.danger{background:#fdecef;border-color:#f5b5c0}",
  ".card.warn{background:#fff6e6;border-color:#f2d79a}",
  ".card.ok{background:#e9f7ef;border-color:#a9dcc0}",
  "section{background:#fff;border:1px solid #e4e4ef;border-radius:.5rem;",
  "padding:1rem 1.25rem;margin-bottom:1.25rem}",
  "h2{margin:.2rem 0 .75rem;font-size:1.1rem}.count{background:#eee;border-radius:1rem;",
  "padding:0 .5rem;font-size:.85rem;color:#444}.scroll{overflow-x:auto}",
  "table{border-collapse:collapse;width:100%;font-size:.9rem}",
  "th,td{text-align:left;padding:.4rem .6rem;border-bottom:1px solid #eee;vertical-align:top}",
  "th{color:#555;font-weight:600}.num{text-align:right}.loc{color:#888;font-size:.8rem}",
  "section.danger h2{color:#c0264a}a{color:#1b6ec2}.ok-msg{color:#1a7f47;font-weight:600}",
  "footer{color:#888;font-size:.8rem;margin-top:2rem}", sep = "")

# Self-contained (no CDN) client-side sort + filter for the report tables.
REPORT_JS <- paste0(
  "document.querySelectorAll('table').forEach(function(t){",
  "t.querySelectorAll('th').forEach(function(th,ci){th.style.cursor='pointer';",
  "th.title='Sort';th.addEventListener('click',function(){",
  "var tb=t.tBodies[0];var rows=Array.prototype.slice.call(tb.rows);",
  "var asc=th.getAttribute('data-asc')!=='1';th.setAttribute('data-asc',asc?'1':'0');",
  "rows.sort(function(a,b){var x=a.cells[ci].innerText.trim();var y=b.cells[ci].innerText.trim();",
  "var nx=parseFloat(x),ny=parseFloat(y);",
  "if(!isNaN(nx)&&!isNaN(ny))return asc?nx-ny:ny-nx;",
  "return asc?x.localeCompare(y):y.localeCompare(x);});",
  "rows.forEach(function(r){tb.appendChild(r);});});});});",
  "var f=document.getElementById('rt-filter');if(f){f.addEventListener('input',function(){",
  "var q=f.value.toLowerCase();document.querySelectorAll('tbody tr').forEach(function(r){",
  "r.style.display=r.innerText.toLowerCase().indexOf(q)>-1?'':'none';});});}"
)

REPORT_JS_CSS <- paste0(
  ".filter{width:100%;max-width:24rem;padding:.5rem .7rem;margin:.5rem 0 1rem;",
  "border:1px solid #ccc;border-radius:6px;font-size:1rem}",
  "th{user-select:none}th:hover{text-decoration:underline}"
)

#' @noRd
render_html <- function(x, title) {
  cnt <- result_counts(x)
  card <- function(v, l, cls = "") {
    sprintf(paste0("<div class='card %s'><div class='v'>%d</div>",
                   "<div class='l'>%s</div></div>"), cls, v, l)
  }
  cards <- paste0(
    "<div class='cards'>",
    card(cnt$flagged, "flagged", "danger"),
    card(cnt$possible, "possible", "warn"),
    card(cnt$notice, "other notice"),
    card(cnt$clean, "clean", "ok"),
    card(cnt$unchecked, "unchecked"),
    "</div>"
  )
  fl <- retracted(x, "flagged")
  ps <- retracted(x, "possible")
  nt <- notice_rows(x)
  sections <- ""
  if (nrow(fl)) sections <- paste0(sections, html_section("Flagged citations", fl, "danger"))
  if (nrow(ps)) {
    sections <- paste0(sections,
                       html_section("Possible matches (verify manually)", ps, "warn"))
  }
  if (nrow(nt)) sections <- paste0(sections, html_section("Other notices", nt))
  if (!nrow(fl) && !nrow(ps)) {
    sections <- paste0(sections, "<p class='ok-msg'>",
                       html_escape(clean_message(cnt)), "</p>")
  }
  has_rows <- nrow(fl) > 0 || nrow(ps) > 0 || nrow(nt) > 0
  filter_input <- if (has_rows) {
    paste0("<input id='rt-filter' class='filter' type='search' ",
           "placeholder='Filter references...' aria-label='Filter references'>")
  } else {
    ""
  }
  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", html_escape(title), "</title><style>", REPORT_CSS, REPORT_JS_CSS,
    "</style></head><body>",
    "<h1>", html_escape(title), "</h1>",
    sprintf("<p class='meta'>Generated %s. %d reference%s checked.</p>",
            html_escape(format(Sys.time())), cnt$n, if (cnt$n == 1L) "" else "s"),
    cards, filter_input, sections,
    "<footer>Created with the retraction R package. Sources: ",
    html_escape(sources_used(x)), ".</footer>",
    "<script>", REPORT_JS, "</script>",
    "</body></html>"
  )
}

# Escape a value for a Markdown table cell: pipes and newlines break the table.
#' @noRd
md_cell <- function(x) {
  x <- blank_na(x)
  x <- gsub("|", "\\|", x, fixed = TRUE)
  gsub("[\r\n]+", " ", x)
}

#' @noRd
md_section <- function(heading, df) {
  rows <- vapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    id <- if (!is.na(r$doi)) sprintf("[%s](https://doi.org/%s)", r$doi, r$doi) else md_cell(r$id)
    when <- if (!is.na(r$retraction_date)) format(r$retraction_date) else ""
    ttl <- if (!is.na(r$matched_title)) md_cell(r$matched_title) else "(no title)"
    sprintf("| %s | %s | %s | %s | %s |", id, ttl, md_cell(r$journal),
            when, md_cell(r$reason))
  }, character(1))
  paste0(
    "\n## ", heading, " (", nrow(df), ")\n\n",
    "| Reference | Title | Journal | Retracted | Reason |\n",
    "|---|---|---|---|---|\n",
    paste(rows, collapse = "\n"), "\n"
  )
}

#' @noRd
render_md <- function(x, title) {
  cnt <- result_counts(x)
  fl <- retracted(x, "flagged")
  ps <- retracted(x, "possible")
  nt <- notice_rows(x)
  out <- paste0(
    "# ", title, "\n\n",
    sprintf("Generated %s. %d reference%s checked.\n\n", format(Sys.time()),
            cnt$n, if (cnt$n == 1L) "" else "s"),
    sprintf(paste0("- **Flagged:** %d\n- **Possible:** %d\n- **Other notice:** %d\n",
                   "- **Clean:** %d\n- **Unchecked:** %d\n"),
            cnt$flagged, cnt$possible, cnt$notice, cnt$clean, cnt$unchecked)
  )
  if (nrow(fl)) out <- paste0(out, md_section("Flagged citations", fl))
  if (nrow(ps)) out <- paste0(out, md_section("Possible matches (verify manually)", ps))
  if (nrow(nt)) out <- paste0(out, md_section("Other notices", nt))
  if (!nrow(fl) && !nrow(ps)) out <- paste0(out, "\n", clean_message(cnt), "\n")
  out
}

#' Render a retraction result as an HTML or Markdown report
#'
#' Produces a shareable, self-contained report. HTML output embeds its own CSS
#' and needs no other software to view.
#'
#' @param x A [`retraction_result`][print.retraction_result].
#' @param output_file Output path. When `NULL`, a temporary file is used.
#' @param format "html" (default) or "md".
#' @param title Report title.
#' @param open Open the report in a browser (interactive HTML only).
#' @return Invisibly, the path to the written report.
#' @examples
#' \donttest{
#' res <- check_dois("10.1016/S0140-6736(97)11096-0")
#' report <- render_report(res, tempfile(fileext = ".html"))
#' }
#' @export
render_report <- function(x, output_file = NULL, format = c("html", "md"),
                          title = "Retraction report", open = FALSE) {
  if (!inherits(x, "retraction_result")) {
    cli::cli_abort("{.arg x} must be a {.cls retraction_result} (from a {.code check_*()} call).")
  }
  format <- match.arg(format)
  if (is.null(output_file)) {
    output_file <- tempfile(fileext = if (format == "md") ".md" else ".html")
  }
  content <- if (format == "html") render_html(x, title) else render_md(x, title)
  writeLines(content, output_file, useBytes = TRUE)
  if (isTRUE(open) && interactive()) utils::browseURL(output_file)
  invisible(output_file)
}
