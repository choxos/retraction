## Build data/retraction_example.rda
##
## A small, stable set of example references used in documentation and tests so
## that examples run without touching the network. One reference is retracted;
## the others are controls.

retraction_example <- data.frame(
  doi = c(
    "10.1016/S0140-6736(97)11096-0",
    "10.1038/s41586-020-2649-2",
    "10.1126/science.aac4716"
  ),
  title = c(
    "Ileal-lymphoid-nodular hyperplasia, non-specific colitis, and pervasive developmental disorder in children",
    "Structure of the SARS-CoV-2 spike glycoprotein",
    "Estimating the reproducibility of psychological science"
  ),
  year = c(1998L, 2020L, 2015L),
  note = c(
    "Retracted by The Lancet in 2010",
    "Control: not retracted",
    "Control: not retracted"
  ),
  stringsAsFactors = FALSE
)

usethis::use_data(retraction_example, overwrite = TRUE)
