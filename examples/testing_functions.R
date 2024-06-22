# Loading the package
library(NIRSVascMuscle)

# Testing functions using the dummy data of NIRSVascMuscle:
# Test function: clean
path <- system.file("extdata", package = "NIRSVascMuscle")
winsize <- 20
vars <- c("TOI_1", "TOI_2")
cleaned_files <- clean(path, winsize, vars)

# Test function sliceslice
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