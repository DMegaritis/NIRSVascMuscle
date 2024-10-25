#' Filter Data Using Savitzky-Golay Filter
#'
#' This function applies the Savitzky-Golay filter to smooth the data in specified variables
#' of data frames stored in a list or CSV files from a path.
#'
#' @param path_or_list Path or list. If path, the file path where the CSV files are located.
#'                     If list, a list of data frames.
#' @param vars Character vector. The names of the variables (columns) in the data frames to be filtered.
#' @return A list of filtered data frames.
#' @examples
#' # Example usage with file path:
#' path <- "path/to/csv/files"
#' vars <- c("var1", "var2", "var3")
#' filtered_data <- filter_smooth(path, vars)
#'
#' # Example usage with list of data frames:
#' data_list <- list(data1, data2, data3)
#' vars <- c("var1", "var2")
#' filtered_smoothed <- filter_smooth(data_list, vars)
#' @import signal
#' @import dplyr
#' @import zoo
#' @export
#' @seealso \code{\link{sgolayfilt}}
#' @references
#' Savitzky A, Golay MJE (1964). "Smoothing and Differentiation of Data by Simplified Least Squares Procedures."
#' Analytical Chemistry, 36(8), 1627-1639. doi: 10.1021/ac60214a047

filter_smooth <- function(path_or_list, vars) {

  if (is.list(path_or_list)) {
    for (i in seq_along(path_or_list)) {
      data <- path_or_list[[i]]
      filtered_files_list <- list()

      for (var in vars) {
        if (var %in% colnames(data)) {
          # proceed
        }
        else {
          stop("var should be column name in the data.")
        }

        filtered <- sgolayfilt(data[[var]], p = 5, n = 15)
        data[[var]] <- filtered
        data[[var]] <- rollmean(data[[var]], k = 10, fill = NA, align = "right")

      }

      filtered_files_list[[file]] <- data
    }
  } else {
    file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)
    filtered_files_list <- list()

    for (file in file_list) {
      data <- read.csv(file)

      for (var in vars) {
        if (var %in% colnames(data)) {
          # proceed
        }
        else {
          stop("var should be column name in the data.")
        }

        filtered <- sgolayfilt(data[[var]], p = 5, n = 15)
        data[[var]] <- filtered
        data[[var]] <- rollmean(data[[var]], k = 10, fill = NA, align = "right")

      }

      filtered_files_list[[file]] <- data
    }
  }

  return(filtered_files_list)

}
