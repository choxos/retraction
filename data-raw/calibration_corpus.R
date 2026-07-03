# Build the labeled calibration corpus used to validate the title-matching
# thresholds. Not shipped (data-raw is .Rbuildignore'd); the resulting CSV is
# shipped in inst/extdata. Run once, online, after retraction_sync().
#
#   Rscript data-raw/calibration_corpus.R

suppressMessages(devtools::load_all(quiet = TRUE))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
set.seed(42)

N <- 200L

## Positives: retracted records sampled from the local snapshot -------------
snap <- retraction:::load_snapshot()
stopifnot(!is.null(snap))
ok <- !is.na(snap$title) & nchar(snap$title) >= 25 &
  !is.na(snap$original_paper_doi) & !is.na(snap$original_paper_date)
pos_idx <- sample(which(ok), N)
pos <- data.frame(
  label = "retracted",
  title = snap$title[pos_idx],
  year = substr(snap$original_paper_date[pos_idx], 1, 4),
  author = snap$author[pos_idx],
  doi = snap$original_paper_doi[pos_idx],
  stringsAsFactors = FALSE
)

## Negatives: non-retracted works sampled from OpenAlex ---------------------
fetch_openalex_clean <- function(n, seed) {
  url <- paste0(
    "https://api.openalex.org/works?",
    "filter=is_retracted:false,has_doi:true,type:article,",
    "title.search:study&per-page=", n, "&sample=", n, "&seed=", seed,
    "&select=doi,title,publication_year,authorships"
  )
  res <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  do.call(rbind, lapply(res$results, function(w) {
    au <- vapply(w$authorships, function(a) a$author$display_name %||% "", character(1))
    data.frame(
      label = "clean",
      title = w$title %||% NA_character_,
      year = as.character(w$publication_year %||% NA),
      author = paste(au, collapse = "; "),
      doi = sub("^https?://doi.org/", "", w$doi %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }))
}
neg <- fetch_openalex_clean(N, 42)
neg <- neg[!is.na(neg$title) & nchar(neg$title) >= 25, ]

## Perturbed positives: realistic citation-title variation -----------------
# Drop ~15% of words and lowercase, to probe the fuzzy threshold with titles
# that are close but not identical to the corpus record.
perturb <- function(title) {
  w <- strsplit(title, "\\s+")[[1]]
  if (length(w) < 6) return(tolower(title))
  keep <- sort(sample(length(w), ceiling(length(w) * 0.85)))
  tolower(paste(w[keep], collapse = " "))
}
pos_fuzzy <- pos
pos_fuzzy$label <- "retracted_fuzzy"
pos_fuzzy$title <- vapply(pos$title, perturb, character(1))

corpus <- rbind(pos, pos_fuzzy, neg)
corpus <- corpus[!is.na(corpus$title), ]

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(corpus, "inst/extdata/calibration_corpus.csv", row.names = FALSE)
cat(sprintf("Wrote %d rows (%d retracted, %d fuzzy, %d clean)\n",
            nrow(corpus), sum(corpus$label == "retracted"),
            sum(corpus$label == "retracted_fuzzy"), sum(corpus$label == "clean")))
