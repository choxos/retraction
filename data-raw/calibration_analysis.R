# Validate the title-matching thresholds against the labeled corpus. For each
# reference it computes the single best title similarity in the corpus (one scan
# per row) and whether that match clears the strict "title_exact" gate, then
# reports flag precision/recall and a fuzzy-threshold sweep. Requires a local
# snapshot (retraction_sync()) and the corpus from calibration_corpus.R.
#
#   Rscript data-raw/calibration_analysis.R

suppressMessages(devtools::load_all(quiet = TRUE))
snap <- retraction:::load_snapshot()
stopifnot(!is.null(snap))
corpus <- utils::read.csv("inst/extdata/calibration_corpus.csv", stringsAsFactors = FALSE)
n <- nrow(corpus)

best_sim <- numeric(n)
is_flag <- logical(n)   # would fire the strict title_exact flag (asserted retracted)
for (i in seq_len(n)) {
  rt <- normalize_title(corpus$title[i])
  sims <- retraction:::title_similarity(rt, snap$norm_title)
  b <- which.max(sims)
  best_sim[i] <- sims[b]
  ref <- retraction:::as_reference(title = corpus$title[i], year = corpus$year[i],
                                   author = corpus$author[i])
  it <- as.list(snap[b, ])
  is_flag[i] <- retraction:::is_exact_metadata(sims[b], ref, it)
}

label <- corpus$label
is_ret <- label %in% c("retracted", "retracted_fuzzy")
is_clean <- label == "clean"

## Strict flag (title_exact) confusion --------------------------------------
tp <- sum(is_ret & is_flag)
fp <- sum(is_clean & is_flag)
fn <- sum(is_ret & !is_flag)
tn <- sum(is_clean & !is_flag)
precision <- tp / max(1, tp + fp)
recall <- tp / max(1, tp + fn)

cat(sprintf("Corpus: %d rows (%d exact-title retracted, %d perturbed retracted, %d clean)\n",
            n, sum(label == "retracted"), sum(label == "retracted_fuzzy"), sum(is_clean)))
cat(sprintf("\n=== Strict flag (title_exact, sim >= 0.985 + year + first author) ===\n"))
cat(sprintf("TP=%d FP=%d FN=%d TN=%d\n", tp, fp, fn, tn))
cat(sprintf("precision=%.3f  recall=%.3f  clean false-flag rate=%.3f\n",
            precision, recall, fp / max(1, sum(is_clean))))
cat("\nFlag rate by label:\n")
print(round(tapply(is_flag, label, mean), 3))

## Fuzzy-threshold sweep: how the "possible match" set behaves ---------------
# A fuzzy match at threshold t surfaces a row as "possible" (matched, not
# asserted). Shows the precision/recall of *surfacing* retracted refs and the
# rate at which clean refs are surfaced for review.
cat("\n=== Fuzzy threshold sweep (matched = best_sim >= t) ===\n")
sweep <- do.call(rbind, lapply(seq(0.80, 0.98, 0.02), function(t) {
  matched <- best_sim >= t
  data.frame(
    threshold = t,
    ret_surfaced = round(mean(matched[is_ret]), 3),
    clean_surfaced = round(mean(matched[is_clean]), 3),
    match_precision = round(sum(matched & is_ret) / max(1, sum(matched)), 3)
  )
}))
print(sweep, row.names = FALSE)

sim_q <- round(sapply(split(best_sim, label), stats::quantile,
                      probs = c(.5, .9, .99)), 3)
cat("\nBest-similarity distribution by label (quantiles):\n")
print(sim_q)

saveRDS(list(n = n, flag = c(tp = tp, fp = fp, fn = fn, tn = tn,
                             precision = precision, recall = recall),
             flag_rate = round(tapply(is_flag, label, mean), 3),
             sweep = sweep, sim_quantiles = sim_q),
        "data-raw/calibration_results.rds")
cat("\nSaved data-raw/calibration_results.rds\n")
