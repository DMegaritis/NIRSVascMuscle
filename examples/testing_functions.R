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

# Test function: filter_smooth
# this function works with both paths and lists (error if there are missing data)
path <- system.file("extdata", package = "NIRSVascMuscle")
vars <- c("TOI_1", "TOI_2")
filtered_smoothed <- filter_smooth(path, vars)

# Single_decay
path_or_list <- filtered_smoothed
vars <- "TOI_1"
event_column <- "Comment"
start_event <- "EVNT13 "
transient_phase <- "on"
eyeball_data <- "yes"
plot_fitted <- "no"
decay_start <- 10
y_Bas_user <- 70
A_p_user <- 15
T_Dp_user <- 9
tau_p_user <- 5
decay <- single_decay(path_or_list, vars, event_column, start_event, transient_phase, eyeball_data, plot_fitted, decay_start, y_Bas_user, A_p_user, T_Dp_user, tau_p_user)

