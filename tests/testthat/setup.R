# Keep the test suite off the network.
options(retraction.check_freshness = FALSE)

# A deterministic backend so the full check pipeline can be exercised offline.
# It flags any DOI containing "6736" (the Wakefield example) as retracted.
register_backend("testfake", function(ref, ctx) {
  if (is_nonempty_string(ref$doi) && grepl("6736", ref$doi)) {
    new_hit("testfake", 1L, checked = TRUE, matched = TRUE, status = "retracted",
            doi = ref$doi, title = "Retracted example", nature = "Retraction",
            retraction_date = as.Date("2010-02-06"), reason = "+Testing",
            matched_on = "original_doi", match_type = "doi_exact",
            confidence = 1, record_id = "4036", journal = "Lancet")
  } else {
    new_hit("testfake", 1L, checked = TRUE)
  }
}, priority = 1L)
