
#' Slice Data Based on Events and Transient Phase
#'
#' This function slices each dataset in a list of data frames based on specified events and transient phases.
#' It selects rows around the events depending on whether the transient phase is "on" or "off".
#'
#' @param file_list A list of data frames containing the datasets.
#' @param period_col The column name in the data frames containing the periods or events.
#' @param events A list of event identifiers to select rows around.
#' @param sampling_freq The sampling frequency of the NIRS data.
#' @param data_time Time duration (in seconds) of data to select around each event.
#' @param transient_phase Character indicating the transient phase: "on" or "off".
#' @return A list of sliced data frames, where each element corresponds to a sliced dataset from each input data frame.
#' @examples
#' # Example usage:
#' file_list <- cleaned_files
#' period_col <- "timestamp"
#' events <- list("event1", "event2")
#' sampling_freq <- 100  # Assuming 100 rows per second
#' data_time <- 10  # Select 10 seconds of data around each event
#' transient_phase <- "off"  # Select data before events
#' sliced_data <- slice(file_list, period_col, events, sampling_freq, data_time, transient_phase)
#' @import zoo
#' @import dplyr
#' @import tidyr
#' @export

slice <- function(file_list, period_col, events, sampling_freq, data_time, transient_phase) {
  file_list <- file_list

  if (!is.list(file_list)) stop("file_list must be a list of data frames.")
  if (!is.list(events)) stop("events must be a list")
  if (!is.numeric(sampling_freq) || length(sampling_freq) != 1) stop("sampling_freq must be a single numeric value.")
  if (!is.numeric(data_time) || length(data_time) != 1) stop("data_time must be a single numeric value.")
  # Ensuring transient_phase is "on" or "off"
  transient_phase <- match.arg(transient_phase, choices = c("on", "off"))

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
    data_length <- data_time * sampling_freq
    if (transient_phase == "off") {
      rows_to_select <- unlist(lapply(evnt_rows, function(x) max(1, x - data_length):x))
      data_slice <- data[unique(rows_to_select), ]
      # adding index to fill the event
      n <- nrow(data_slice)
      values <- rep(1:length(events), each = (sampling_freq*data_time + 1), length.out = n)
      data_slice$idx <- values
      data_slice[period_col][data_slice[period_col] == ""] <- NA
      data_slice <- data_slice %>%
        group_by(idx) %>%
        fill(period_col, .direction = "downup") %>%
        ungroup()

    } else if (transient_phase == "on") {
      rows_to_select <- unlist(lapply(evnt_rows, function(x) max(1, x):(x + data_length)))
      data_slice <- data[unique(rows_to_select), ]
      # filling the event to be used for next step
      n <- nrow(data_slice)
      values <- rep(1:length(events), each = (sampling_freq*data_time + 1), length.out = n)
      data_slice$idx <- values
      data_slice[period_col][data_slice[period_col] == ""] <- NA
      data_slice <- data_slice %>%
        group_by(idx) %>%
        fill(period_col, .direction = "updown") %>%
        ungroup()

    }

    cleaned_sliced_file_list[[i]] <- data_slice
  }
  return(cleaned_sliced_file_list)
}

