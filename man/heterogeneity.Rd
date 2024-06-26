% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heterogeneity.R
\name{heterogeneity}
\alias{heterogeneity}
\title{Calculate Heterogeneity of variable Across Channels}
\usage{
heterogeneity(data, vars, cv_vars, ID, time_point)
}
\arguments{
\item{data}{A data frame containing the NIRS data.}

\item{vars}{Character vector. The column names to be selected from the data, including the variables for all 4 channels,
the participant identifier, and the time period. All those are required for data manipulation before calculation of the cv.}

\item{cv_vars}{Character vector. The column names for the variables in the different channels that will be used to calculate heterogeneity.}

\item{ID}{Character. The column name for the participant identifier.}

\item{time_point}{Character. The column name for the time period.}
}
\value{
A data frame containing the calculated coefficient of variation (CV) for each participant and time point, along with a new column indicating the population.
}
\description{
This function pivots the NIRS data into a long format,
and calculates the coefficient of variation (CV) to measure heterogeneity across different channels.
As previously, filename may include the ID in the 7 first instances of the file name (i.e., 001-001)
while the first 3 instances may indicate the population (i.e., 001).
}
\examples{
# Example usage:
data <- read.csv("path/to/your/data.csv")  # Assuming the data is already loaded into the R environment
vars <- c("TOI_1", "TOI_2", "TOI_3", "TOI_4", "ID", "Ex")
cv_vars <- c("TOI_1", "TOI_2", "TOI_3", "TOI_4")
ID <- "ID"
time_point <- "Ex"
result <- heterogeneity(data, vars, cv_vars, ID, time_point)
}
