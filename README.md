# NIRSVascMuscle

NIRSVascMuscle is an R package designed for analyzing Near-Infrared Spectroscopy (NIRS) data in muscle physiology research. This package provides tools for data smoothing, cleaning, event slicing, aggregation, heterogeneity, and on-off transient analysis across multiple channels.

## General Description

This is a package to perform analyses on Near-Infrared Spectroscopy (NIRS) data. The heterogeneity analyses are based on the paper:

- Vogiatzis, I., Habazettl, H., Louvaris, Z., Andrianopoulos, V., Wagner, H., Zakynthinos, S., & Wagner, P. D. (2015). A method for assessing heterogeneity of blood flow and metabolism in exercising normal human muscle by near-infrared spectroscopy. Journal of applied physiology (Bethesda, Md. : 1985), 118(6), 783-793. https://doi.org/10.1152/japplphysiol.00458.2014

Additionally, the kinetics analyses implemented equations from the following original papers:

- Bauer, T. A., Brass, E. P., Barstow, T. J., & Hiatt, W. R. (2007, May). Skeletal muscle StO2 kinetics are slowed during low work rate calf exercise in peripheral arterial disease. European Journal of Applied Physiology, 100(2), 143-151. https://doi.org/10.1007/s00421-007-0412-0 

- Grassi, B., Pogliaghi, S., Rampichini, S., Quaresima, V., Ferrari, M., Marconi, C., & Cerretelli, P. (2003, Jul). Muscle oxygenation and pulmonary gas exchange kinetics during cycling exercise on-transitions in humans. J Appl Physiol (1985), 95(1), 149-158. https://doi.org/10.1152/japplphysiol.00695.2002 


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
```

## Running the Functions

```r
# Loading the package
# Loading the package
library(NIRSVascMuscle)

# Testing functions using the dummy data of NIRSVascMuscle:
# Test function: clean
path <- system.file("extdata", package = "NIRSVascMuscle")
winsize <- 20
vars <- c("TOI_1", "TOI_2")
cleaned_files <- clean(path, winsize, vars)

# Test function: filter_sg
path <- system.file("extdata", package = "NIRSVascMuscle")
vars <- c("TOI_1", "TOI_2")
filtering <- filter_sg(path, vars)

# Test function: slice
files <- cleaned_files
period_col <- "Comment"
events <- list("EVNT1 ", "EVNT2 ")
sampling_freq <- 5
data_time <- 30
transient_phase <- "on"
cleaned_sliced <- slice(files, period_col, events, sampling_freq, data_time, transient_phase)

# Aggregate
aggregated <- aggregate_NIRS(cleaned_sliced)

# Heterogeneity calculator
vars <- c("TOI_1", "TOI_2", "TOI_3", "TOI_4", "ID", "Comment")
cv_vars <- c("TOI_1", "TOI_2", "TOI_3", "TOI_4")
ID <- "ID"
time_point <- "Comment"
hetero <- heterogeneity(aggregated, vars, cv_vars, ID, time_point)
```
