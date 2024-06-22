# NIRSVascMuscle

## Installation

To install the NIRSVascMuscle package from GitHub, you can use the following code:

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

library(devtools)

devtools::install_github("DMegaritis/NIRSVascMuscle")
library(NIRSVascMuscle)

cat("NIRSVascMuscle package loaded successfully.\n")
