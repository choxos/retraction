## Regenerate network fixtures under tests/testthat/fixtures/ from the live APIs.
## Run manually when the upstream response shape changes. Not run on CRAN.

devtools::load_all()
dir <- "tests/testthat/fixtures"
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# Xera search items for a known retracted DOI (used to test the adapter).
items <- xera_search_doi("10.1016/S0140-6736(97)11096-0")
saveRDS(items, file.path(dir, "xera_search_wakefield.rds"))

# Xera detail record (used to test enrichment).
detail <- xera_paper("4036")
saveRDS(detail, file.path(dir, "xera_detail_4036.rds"))

message("Wrote fixtures: ", length(items), " search items; detail record present: ",
        !is.null(detail))
