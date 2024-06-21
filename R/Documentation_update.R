#' Set Working Directory and Document the Package
#'
#' This script sets the working directory to the root of the package,
#' generates the documentation using roxygen2, and performs a package check.
#'
#' @description
#' Set the working directory to the root of your package, generate the documentation,
#' and check the package for any issues.
#'
#' @examples
#' # Set the working directory to the root of your package
#' setwd("/Users/dimitrismegaritis/Desktop/NIRSVascMusle")
#'
#' # Document the package
#' devtools::document()
#'
#' # Check the package
#' devtools::check()

# Set working directory to the root of your package
setwd("/Users/dimitrismegaritis/Desktop/NIRSVascMusle")

# Document the package
devtools::document()

# Check the package
devtools::check()
