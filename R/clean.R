
#' This script includes different functions for data cleaning and manipulation

#' Clean NIRS Data Using Rolling Z-Scores
#'
#' This function reads multiple CSV files containing NIRS data, applies a rolling z-score
#' method to identify and remove outliers, and returns a list of cleaned data frames.
#' Filename may include the ID in the 7 first instances of the file name (i.e., 001-001)
#' while the first 3 instances may indicate the population (i.e., 001).
#' Columns are added in each file indicating ID and population.
#'
#' @param path Character. The file path where the CSV files are located.
#' @param winsize Integer. The window size to be used for calculating the rolling z-scores.
#' @param vars Character vector. The names of the variables (columns) in the data frames to be cleaned.
#' @return A list of cleaned data frames.
#' @examples
#' # Example usage:
#' path <- "path/to/csv/files"
#' winsize <- 50
#' vars <- c("var1", "var2", "var3")
#' cleaned_data <- clean(path, winsize, vars)
#' @import zoo
#' @import dplyr
#' @import tidyr
#' @export

clean <- function(path, winsize, vars) {
  file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

  window_size <- winsize

  cleaned_files_list <- list()

  for (file in file_list) {
    data <- read.csv(file)
    # check if all selected column are continuous
    if (all(vars %in% colnames(data))) {
      if (all(sapply(data[vars], is.numeric))) {
        cols <- vars
      } else {
        stop("All vars should be continuous (numeric) variables.")
      }
    } else {
      stop("vars should be column names in the data.")
    }

    # loop to remove outliers to the whole dataframe
    for (col in cols) {
      # Rolling mean and standard deviation, handling NAs
      rolling_mean <- rollapply(data[[col]], width = window_size, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "center", partial = TRUE)
      rolling_std <- rollapply(data[[col]], width = window_size, FUN = function(x) sd(x, na.rm = TRUE), fill = NA, align = "center", partial = TRUE)

      # Rolling z-scores
      z_scores <- (data[[col]] - rolling_mean) / rolling_std

      # Identify outliers (z-scores with absolute value greater than 3)
      outliers <- abs(z_scores) > 2

      # Replace outliers with NA
      data[[col]][outliers] <- NA

      # add ID column from filename
      # add population column (outcome)
      ID <- substr(basename(file), 1, 7)
      population <- substr(basename(file), 1, 3)
      data$ID <- ID
      data$population <- population
    }
    cleaned_files_list[[file]] <- data
  }
  return(cleaned_files_list)
}
