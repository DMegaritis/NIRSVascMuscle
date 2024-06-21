#' Aggregate Cleaned Datasets
#'
#' This function aggregates multiple cleaned datasets stored in a list of data frames into a single data frame.
#'
#' @param cleaned_sliced_files A list containing cleaned and sliced data frames from multiple CSV files.
#' @return A single aggregated data frame combining all datasets.
#' @examples
#' # Example usage:
#' cleaned_files <- list(
#'   "file1.csv" = read.csv("path/to/file1.csv"),
#'   "file2.csv" = read.csv("path/to/file2.csv")
#' )
#' aggregated_data <- aggregate(cleaned_files)
#' @import zoo
#' @import dplyr
#' @import tidyr
#' @export


aggregate_NIRS <- function(cleaned_sliced_files) {
  if (!is.list(cleaned_sliced_files)) stop("cleaned_sliced_files must be a list of data frames.")

  merged_data <- do.call(rbind, cleaned_sliced_files)
  return(merged_data)
}
