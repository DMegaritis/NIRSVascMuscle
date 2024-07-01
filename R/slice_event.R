
#' Slice Data Based on Events and Transient Phase
#'
#' This function slices each dataset in a list of data frames based on specified events.
#' Depending on the transient_phase the  data are sliced in a different way. For heterogeneity analyses the last seconds (user defined) before a specific period are sliced.
#' For kinetics analyses the data are sliced around the event depending on the user settings.
#'
#' @param file_list A list of data frames containing the datasets.
#' @param period_col The column name in the data frames containing the periods or events.
#' @param events A list of event identifiers to select rows around.
#' @param sampling_freq The sampling frequency of the NIRS data.
#' @param data_before Time duration (in seconds) of data to select before each event.
#' @param data_after Time duration (in seconds) of data to select after each event.
#' @param transient_phase Character indicating the transient phase: "heterogeneity" or "kinetics". If transient_phase is set to "heterogeneity" the data_after parameter is not used
#' @return A list of sliced data frames, where each element corresponds to a sliced dataset from each input data frame.
#' @examples
#TODO: update examples everywhere
#' # Example usage:
#' file_list <- cleaned_files
#' period_col <- "timestamp"
#' events <- list("event1", "event2")
#' sampling_freq <- 100  # Assuming 100 rows per second
#' data_before <- 10  # Select 10 seconds of data before each event
#' data_after <- 60 # Select 60 seconds of data after ach event
#' transient_phase <- "kinetics"
#' sliced_data <- slice_event(file_list, period_col, events, sampling_freq, data_before, data_after, transient_phase)
#' @import zoo
#' @import dplyr
#' @import tidyr
#' @export

slice_event <- function(file_list, period_col, events, sampling_freq, data_before, data_after, transient_phase) {
  file_list <- file_list

  if (!is.list(file_list)) stop("file_list must be a list of data frames.")
  if (!is.list(events)) stop("events must be a list")
  if (!is.numeric(sampling_freq) || length(sampling_freq) != 1) stop("sampling_freq must be a single numeric value.")
  if (!is.numeric(data_before)) stop("data_before must be a single numeric value.")
  if (!is.numeric(data_after)) stop("data_after must be a single numeric value.")
  # Ensuring transient_phase is "on" or "off"
  transient_phase <- match.arg(transient_phase, choices = c("heterogeneity", "kinetics"))

  cleaned_sliced_file_list <- list()

  for (i in seq_along(file_list)) {
    data <- file_list[[i]]

    if (period_col %in% colnames(data)) {
      if (sapply(data[period_col], is.character)) {
        # proceed
      } else {
        stop("Period_col should be a categorical variable.")
      }
    } else {
      stop("period_col should be column name in the data.")
    }

    # selecting relevant markers
    evnt_rows <- which(data$Comment %in% events)
    # creating function to slice a pre-selected number of rows depending on the sampling rate, and the selection of period (on or off transient)
    data_length_before <- data_before * sampling_freq
    if (transient_phase == "heterogeneity") {
      rows_to_select <- unlist(lapply(evnt_rows, function(x) max(1, x - data_length_before):x))
      data_slice <- data[unique(rows_to_select), ]
      # filling the event to be used for next step
      n <- nrow(data_slice)
      values <- rep(1:length(events), each = (sampling_freq*data_before + 1), length.out = n)
      data_slice$idx <- values
      data_slice[period_col][data_slice[period_col] == ""] <- NA
      data_slice <- data_slice %>%
        group_by(idx) %>%
        fill(period_col, .direction = "downup") %>%
        ungroup()

    } else if (transient_phase == "kinetics") {
      data_length_after <- data_after * sampling_freq
      rows_to_select_after <- unlist(lapply(evnt_rows, function(x) max(1, x):(x + data_length_after)))
      rows_to_select_before <- unlist(lapply(evnt_rows, function(x) max(1, x - data_length_before):x))

      combined_rows_to_select <- c(rows_to_select_before, rows_to_select_after)
      unique_rows_to_select <- unique(combined_rows_to_select)
      data_slice <- data[unique_rows_to_select, ]

    }

    cleaned_sliced_file_list[[i]] <- data_slice
  }
  return(cleaned_sliced_file_list)
}

