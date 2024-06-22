# Example on how to load the NIRSVascMuscle package from GitHub

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

library(devtools)

devtools::install_github("DMegaritis/NIRSVascMuscle")
library(NIRSVascMuscle)

cat("NIRSVascMuscle package loaded successfully.\n")
