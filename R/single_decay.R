#' Single Exponential Decay Fitting
#'
#' This function processes data frames or CSV files to fit a single exponential decay model to specified variables.
#' It allows for manual or automatic calculation of decay parameters and provides options to visualize the fitted model.
#' Before calling this function, your data have to be smoothed, the function "filter_smooth" is suggested rather than the function "clean"
#' since no missing values are allowed.
#' @param path_or_list A list of data frames or a directory path containing CSV files to be processed.
#' @param vars A vector of a single column name in the data frames to which the decay model should be applied.
#' @param event_column The column name (as a string) in the data frames that contains the event markers.
#' @param start_event The value in the event column that marks the start of the event.
#' @param time_column The column name indicating the time in the users dataset
#' @param transient_phase An optional parameter to specify the transient phase. Options are "increase" and "decrease". This choice was made since different physiological variables respond differently to the same period. Increase indicates that the variable increases after the start_period while decrease the opposite.
#' @param eyeball_data A string ("yes" or "no") indicating whether to calculate initial decay parameters using simple maths. If "no" the user has to specify the initial decay parameters
#' @param plot_fitted A string ("yes" or "no") indicating whether to plot the fitted model.
#' @param decay_start The starting point (numeric) for the decay fitting. This should be specified in second.
#' @param y_Bas_user The user-defined baseline value (numeric) for the y variable.
#' @param A_p_user The user-defined amplitude value (numeric) for the decay model.
#' @param T_Dp_user The user-defined time delay value (numeric) for the decay model.
#' @param tau_p_user The user-defined time constant value (numeric) for the decay model.
#'
#' @return A data frame containing the fitted parameters for each variable in the input data.
#' @examples
#' # Example usage with a list of data frames:
#' path_or_list <- filtered_smoothed
#' vars <- "TOI_1"
#' event_column <- "Comment"
#' start_event <- "EVNT13 "
#' time_column <- "elpsec"
#' transient_phase <- "decrease"
#' eyeball_data <- "yes"
#' plot_fitted <- "no"
#' decay_start <- 10
#' y_Bas_user <- 70
#' A_p_user <- 15
#' T_Dp_user <- 9
#' tau_p_user <- 5
#'
#' decay <- single_decay(path_or_list, vars, event_column, start_event, time_column, transient_phase, eyeball_data, plot_fitted, decay_start, y_Bas_user, A_p_user, T_Dp_user, tau_p_user)
#' @import ggplot2
#' @import dplyr
#' @import zoo
#' @import minpack.lm
#' @export

single_decay <- function(path_or_list, vars, event_column, start_event, time_column, transient_phase, eyeball_data, plot_fitted, decay_start, y_Bas_user, A_p_user, T_Dp_user, tau_p_user) {

  eyeball_data <- match.arg(eyeball_data, choices = c("yes", "no"))
  plot_fitted <- match.arg(plot_fitted, choices = c("yes", "no"))
  transient_phase <- match.arg(transient_phase, choices = c("decrease", "increase"))

  # Specifying function to be called below in different iterations of the if statement
  process_data <- function(data, vars, event_column, start_event, time_column, transient_phase, eyeball_data, plot_fitted, decay_start, y_Bas_user, A_p_user, T_Dp_user, tau_p_user) {
    tryCatch({
      kinetics <- data.frame(
        variable = character(0),
        ID = character(0),
        y_baseline = numeric(0),
        amplitide = numeric(0),
        time_delay = numeric(0),
        tau = numeric(0),
        mrt = numeric(0),
        rse = numeric(0),
        decay = character(0)
      )

      for (var in vars) {
        if (var %in% colnames(data)) {
          # proceed
        } else {
          stop("var should be column name in the data.")
        }

        if (eyeball_data == "yes") {
          # manual calculation of components
          # start of period
          start <- which(data[[event_column]] == start_event)
          start <- as.numeric(start)

          # y_Bas
          y_Bas <- mean(data[start:(start - 25), var], na.rm = TRUE)

          # time delay
          if (transient_phase == "decrease") {
            percentage_decrease <- 0.015
            index_change <- as.numeric(which((data[[var]] - y_Bas) / y_Bas < -percentage_decrease)[1])
          } else if (transient_phase == "increase") {
            percentage_increase <- 0.015
            index_change <- as.numeric(which((data[[var]] - y_Bas) / y_Bas > percentage_increase)[1])
          }

          # plateau value and index
          signal <- data[index_change:nrow(data), ]
          threshold <- 0.03

          find_plateau_start <- function(signal, threshold, window_size = 50) {
            for (i in 2:(length(signal) - window_size)) {
              rolling_mean <- mean(signal[(i + 1):(i + window_size)])

              if (abs((signal[i] - rolling_mean) / rolling_mean) <= threshold) {
                return(i)
              }
            }
            return(NULL)
          }

          plateau_start <- as.numeric(find_plateau_start(signal[[var]], threshold, 50))

          # Identifying the start of the plateau
          plateau_index <- as.numeric(index_change + plateau_start)

          # Calculating plateau value
          plateau <- data[plateau_index:nrow(data), ]
          y_plateau <- mean(plateau[[var]], na.rm = TRUE)

          # Calculating amplitude (Ap)
          Ap <- abs(y_plateau - y_Bas)

          # Calculating TD
          TD <- data[["elpsec"]][index_change] - data[["elpsec"]][start]

          # Calculating time constant (tp_time)
          y_plateau_start <- data[[var]][plateau_index]
          y_63 <- y_Bas - 0.63*Ap
          tp_index <- which.min(abs(data[[var]] - y_63))
          tp_time <- abs(data[["elpsec"]][index_change] - data[["elpsec"]][tp_index])

          # Calculating mean response time (mrt)
          mrt <- tp_time + TD

          # Adding index starting from 0 during start (might be used at a later stage)
          data$index <- c(seq(-start + 1, -1), seq(0, nrow(data) - start))
          data <- data[start:nrow(data), ]

          # Resetting elpsec (time) to start from 0
          first_instance <- data[["elpsec"]][1]
          data[["elpsec"]] <- data[["elpsec"]] - first_instance

          decay_start_index <- index_change - start
          decay_start <- data[["elpsec"]][decay_start_index]

          # Fitting the single decay function
          if (transient_phase == "decrease") {
            fit <- nlsLM(as.formula(paste(var, "~ ifelse(", "elpsec", "<= decay_start, y_Bas, y_Bas - A_p * (1 - exp(-(", "elpsec", " - T_Dp) / tau_p)))")),
                         data = data,
                         start = c(y_Bas = y_Bas, A_p = Ap, T_Dp = TD, tau_p = tp_time),
                         lower = c(y_Bas = 0, A_p = 0, T_Dp = 0, tau_p = 0),
                         upper = c(y_Bas = 200, A_p = 100, T_Dp = 100, tau_p = 50),
                         algorithm = "port",
                         control = nls.lm.control(maxiter = 1024))
          } else if (transient_phase == "increase") {
            fit <- nlsLM(as.formula(paste(var, "~ ifelse(", "elpsec", "<= decay_start, y_Bas, y_Bas + A_p * (1 - exp(-(", "elpsec", " - T_Dp) / tau_p)))")),
                         data = data,
                         start = c(y_Bas = y_Bas, A_p = Ap, T_Dp = TD, tau_p = tp_time),
                         lower = c(y_Bas = 0, A_p = 0, T_Dp = 0, tau_p = 0),
                         upper = c(y_Bas = 200, A_p = 100, T_Dp = 100, tau_p = 50),
                         algorithm = "port",
                         control = nls.lm.control(maxiter = 1024))
          }


          # Getting the fitted values
          data$fitted <- predict(fit)

          if (plot_fitted == "yes") {
            ggplot(data, aes(x = elpsec, y = TOI_1)) +
              geom_point(color = "blue", alpha = 0.5) +
              geom_line(aes(y = fitted), color = "red") +
              labs(title = "Non-linear Least Squares Fit",
                   x = "elpsec",
                   y = "TOI_1") +
              theme_minimal()
          }

          # Summary fit provides coefficients to be extracted
          summary <- summary(fit)
          estimates <- summary$coefficients[, "Estimate"]
          mrt <- estimates[3] + estimates[4]

          # Calculating residuals standard error
          data$residuals <- residuals(fit)
          RSE <- sqrt(sum(data[["residuals"]]^2) / df.residual(fit))

        } else if (eyeball_data == "no") {

          # start of period
          start <- which(data[[event_column]] == start_event)
          start <- as.numeric(start)

          # Adding index starting from 0 during start (might be used at a later stage)
          data$index <- c(seq(-start + 1, -1), seq(0, nrow(data) - start))
          data <- data[start:nrow(data), ]

          # Resetting elpsec (time) to start from 0
          first_instance <- data[["elpsec"]][1]
          data[["elpsec"]] <- data[["elpsec"]] - first_instance

          if (transient_phase == "decrease") {
            # Construct the conditional part of the formula
            ifelse_part <- paste(y_Bas_user, " - ", A_p_user, " * (1 - exp(-(elpsec - ", T_Dp_user, ") / ", tau_p_user, "))", sep = "")

            # Construct the full formula string
            formula_str <- paste(var, "~ ifelse(elpsec <= ", decay_start, ", ", y_Bas_user, ", ", ifelse_part, ")", sep = "")

            fit <- nlsLM(as.formula(formula_str),
                         data = data,
                         start = list(y_Bas = y_Bas_user, A_p = A_p_user, T_Dp = T_Dp_user, tau_p = tau_p_user),
                         lower = c(y_Bas = 0, A_p = 0, T_Dp = 0, tau_p = 0),
                         upper = c(y_Bas = 200, A_p = 100, T_Dp = 100, tau_p = 50),
                         algorithm = "port",
                         control = nls.lm.control(maxiter = 1024))
          } else if (transient_phase == "increase") {
            # Construct the conditional part of the formula
            ifelse_part <- paste(y_Bas_user, " + ", A_p_user, " * (1 - exp(-(elpsec - ", T_Dp_user, ") / ", tau_p_user, "))", sep = "")

            # Construct the full formula string
            formula_str <- paste(var, "~ ifelse(elpsec <= ", decay_start, ", ", y_Bas_user, ", ", ifelse_part, ")", sep = "")

            fit <- nlsLM(as.formula(formula_str),
                         data = data,
                         start = list(y_Bas = y_Bas_user, A_p = A_p_user, T_Dp = T_Dp_user, tau_p = tau_p_user),
                         lower = c(y_Bas = 0, A_p = 0, T_Dp = 0, tau_p = 0),
                         upper = c(y_Bas = 200, A_p = 100, T_Dp = 100, tau_p = 50),
                         algorithm = "port",
                         control = nls.lm.control(maxiter = 1024))
          }

          # Getting the fitted values
          data$fitted <- predict(fit)

          if (plot_fitted == "yes") {
            ggplot(data, aes(x = elpsec, y = TOI_1)) +
              geom_point(color = "blue", alpha = 0.5) +
              geom_line(aes(y = fitted), color = "red") +
              labs(title = "Non-linear Least Squares Fit",
                   x = "elpsec",
                   y = "TOI_1") +
              theme_minimal()
          }

          # Summary fit provides coefficients to be extracted
          summary <- summary(fit)
          estimates <- summary$coefficients[, "Estimate"]
          mrt <- estimates[3] + estimates[4]

          # Calculating residuals standard error
          data$residuals <- residuals(fit)
          RSE <- sqrt(sum(data[["residuals"]]^2) / df.residual(fit))
        }

        # Append results to kinetics data frame
        kinetics <- rbind(kinetics, data.frame(variable = var, ID = ID, y_baseline = estimates[1],
                                               amplitide = estimates[2], time_delay = estimates[3],
                                               tau = estimates[4], mrt = mrt, rse = RSE, decay = "single decay"))

      }
      return(kinetics)


    }, error = function(e) {
      cat("Error occurred: ", conditionMessage(e), "\n")
      traceback
    })
  }

  # Depending on type of path_or_list calling the process_data function
  # Empty list to store results
  results <- list()

  if (is.list(path_or_list)) {
    # Process each data frame in the list
    for (i in seq_along(path_or_list)) {
      #renaming time_column to elpsec
      names(path_or_list[[i]])[names(path_or_list[[i]]) == time_column] <- "elpsec"
      # If loading from a list created in the previous steps, the ID is included as a column in each data frame from the list
      ID <- path_or_list[[1]]$ID[1]
      results[[i]] <- process_data(path_or_list[[i]], vars, event_column, start_event, time_column, transient_phase, eyeball_data, plot_fitted, decay_start, y_Bas_user, A_p_user, T_Dp_user, tau_p_user)

    }
  } else if (is.character(path_or_list) && dir.exists(path_or_list)) {
    # Process each CSV file in the directory
    file_list <- list.files(path = path_or_list, pattern = "\\.csv$", full.names = TRUE)

    for (file in file_list) {
      # Extracting name (ID)
      name <- basename(file)
      ID <- substr(name, 1, 7)
      data <- read.csv(file)
      #renaming time_column to elpsec
      names(data)[names(data) == time_column] <- "elpsec"

      results[[file]] <- process_data(data, vars, event_column, start_event, time_column, transient_phase, eyeball_data, plot_fitted, decay_start, y_Bas_user, A_p_user, T_Dp_user, tau_p_user)
    }
  } else {
    stop("path_or_list must be either a list of data frames or a valid directory path containing CSV files.")
  }

  kinetics_merged <- do.call(rbind, results)
  return(kinetics_merged)
}


