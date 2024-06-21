

#' Calculate Heterogeneity of variable Across Channels
#'
#' This function pivots the NIRS data into a long format,
#' and calculates the coefficient of variation (CV) to measure heterogeneity across different channels.
#' As previously, filename may include the ID in the 7 first instances of the file name (i.e., 001-001)
#' while the first 3 instances may indicate the population (i.e., 001). 

#' @param data A data frame containing the NIRS data.
#' @param vars Character vector. The column names to be selected from the data, including the variables for all 4 channels, 
#' the participant identifier, and the time period. All those are required for data manipulation before calculation of the cv.
#' @param cv_vars Character vector. The column names for the variables in the different channels that will be used to calculate heterogeneity.
#' @param ID Character. The column name for the participant identifier.
#' @param time_point Character. The column name for the time period.
#' @return A data frame containing the calculated coefficient of variation (CV) for each participant and time point, along with a new column indicating the population.
#' @examples
#' # Example usage:
#' data <- read.csv("path/to/your/data.csv")  # Assuming the data is already loaded into the R environment
#' vars <- c("TOI_1", "TOI_2", "TOI_3", "TOI_4", "ID", "Ex")
#' cv_vars <- c("TOI_1", "TOI_2", "TOI_3", "TOI_4")
#' ID <- "ID"
#' time_point <- "Ex"
#' result <- heterogeneity(data, vars, cv_vars, ID, time_point)
#' @import zoo
#' @import dplyr
#' @import tidyr
#' @export


heterogeneity <- function(data, vars, cv_vars, ID, time_point) {
  
  # Checks to ensure presence and type of variables in the data frame
  if (all(vars %in% colnames(data))) {
    selected_vars <- vars
  } else {
    stop("vars should be column names in the data.")
  }
  
  if (all(cv_vars %in% colnames(data))) {
    if (all(sapply(data[cv_vars], is.numeric))) {
      pivot_vars <- cv_vars
    } else {
      stop("All cv_vars should be continuous (numeric) variables.")
    }
  } else {
    stop("cv_vars should be column names in the data.")
  }
  
  if (ID %in% colnames(data)) {
    if (sapply(data[ID], is.character)) {
      # proceed
    } else {
      stop("ID vars should be categorical variable.")
    }
  } else {
    stop("ID should be a column name in the data.")
  }
  
  if (time_point %in% colnames(data)) {
    if (sapply(data[time_point], is.character)) {
      # proceed
    } else {
      stop("time_point vars should be categorical variable.")
    }
  } else {
    stop("time_point should be a column name in the data.")
  }
  
  # Data in long format for calculating the cv across different channels
  data_long <- data %>%
    select(all_of(selected_vars)) %>%
    pivot_longer(cols = all_of(pivot_vars),
                 names_to = "channel",
                 values_to = "CV")
  
  # calculating the heterogeneity and creating new population column
  cv <- data_long %>%
    group_by(!!sym(ID), !!sym(time_point)) %>%
    summarise(cv = ((sd(CV, na.rm = TRUE)) / mean(CV, na.rm = TRUE))) %>%
    mutate(population = substr(ID, 1, 3))
  
  return(cv)
}
