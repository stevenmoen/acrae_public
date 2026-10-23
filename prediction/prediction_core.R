#### Prediction Function ####

#' Prediction with the maxima. 
#' NOTE: The log_sigma code assumes that the beta_2 and beta_4 values are negative. 
#' Note that the first predicted low, fairway, and high values of Q_t are identical as these are generated using a
#' random sampling procedure
#'
#' @param input_data_df - data frame of data that will be used for prediction
#' @param input_param_df - data frame of input estimated parameters that will be used for prediction
#' @param alpha - the confidence level
#' @param r_seed_start - starting point for random seed. Only used in the initialization step. This is not used in the coverage table
#' @param print_flag - print information about the confidence level and dataset size?
#'
#' @return - a data frame with prediction, a table of coverages, and a grand coverage
#' @export
#'
#' @examples - prediction_with_max(input_data_df = data_new[1:50,], input_param_df = est_parameters_original, alpha = 0.05)
prediction_with_max <- function(input_data_df, input_param_df, alpha, r_seed_start = 88500, print_flag = 0,
                                empirical_flag = 1,
                                empirical_frechet_vec = r_frechet_sort) {
  #### Prediction Function: Setup ####
  # Print the confidence level
  if (print_flag == 1){
    print(paste0("The confidence level is ", (1-alpha)*100, "%"))
  }
  # Compute the low, high, and median Y_t values
  if (empirical_flag == 1){
    Y_low <- quantile(empirical_frechet_vec, (alpha/2))
    Y_median <- quantile(empirical_frechet_vec, 0.5)
    Y_high <- quantile(empirical_frechet_vec, 1-(alpha/2))
  } else {
    Y_low <- qfrechet(p = (alpha/2), shape = 1)
    Y_median <- qfrechet(p = 0.5, shape = 1)
    Y_high <- qfrechet(p = 1-(alpha/2), shape = 1)
  }
  # Calculate the number of data points
  n_time_points <- nrow(input_data_df)
  # Calculate the number of locations
  n_locations <- nrow(input_param_df)
  # Print a notification about the number of time points
  if (print_flag == 1){
    print(paste0("There are ", n_time_points, " time points in your dataset."))
  }
  # Print a warning if n_locations is not 120
  if(n_locations != 120){
    print("WARNING: There are not 120 locations in your input data file. This may cause issues with the code.")
  }
  # Generate a storage dataframe with 899*120 = 107880 rows
  predicted_data_df <- as.data.frame(matrix(0, nrow = n_time_points*n_locations, ncol = 9))
  # Generate column names
  colnames(predicted_data_df) <- c("Time ID", "Location ID", "R_pred_low", "R_pred_fairway", "R_pred_high", "logAlpha_pred", "logSigma_pred", "R_actual", "R_local_max_actual")
  # Fill in the key variables
  # Time ID counts the step in the run. There should be 120 locations within each step, and 899 total Time IDs in the 2022 data
  # Location ID counts the location in each run, and within each step. It should constantly go up from 1 to 120.
  predicted_data_df$`Time ID` <- rep(1:n_time_points, each = n_locations)
  predicted_data_df$`Location ID` <- rep(1:n_locations, times = n_time_points)
  # Initialize the big_mat, small_mat, and a storage vector
  big_mat <- matrix(-Inf, nrow = 14, ncol = 12)
  max_mat <- matrix(NA, nrow = 12, ncol = 10)
  sto_vec <- rep(NA, 8)
  max_vec <- rep(NA, n_locations)
  ##### Prediction function: Adding real data, regional maxes, and lags #####
  # Append the real data to the dataframe
  predicted_data_df$R_actual <- as.vector(t(input_data_df))
  # Compute the actual local max for the data
  for (j in 1:n_time_points){
    for (k in 1:n_locations){
      # Put the data into a matrix
      big_mat[2:13, 2:11] <- matrix(predicted_data_df$R_actual[(1 + (j-1)*n_locations): (n_locations + (j-1)*n_locations)], nrow = 12, ncol = 10, byrow = TRUE)
      # Loop through and populate the max_mat
      for (i_ in 2:13){
        for (j_ in 2:11){
          # This takes the maxima around in a square, starting with lower left, and moving clockwise.
          sto_vec <- c(big_mat[i_-1, j_-1], big_mat[i_-1, j_], big_mat[i_-1, j_+1], big_mat[i_, j_+1],
                       big_mat[i_+1, j_+1], big_mat[i_+1, j_], big_mat[i_+1, j_-1], big_mat[i_, j_-1])
          max_mat[i_-1, j_-1] <- max(sto_vec)
        }
      }
      # Now we need to flatten this back into a vector
      max_vec <- c(t(max_mat))
      # Put this in the data frame
      predicted_data_df$R_local_max_actual[(1 + (j-1)*n_locations):(n_locations + (j-1)*n_locations)] <- max_vec
    }
  }
  #### Prediction Function: Computing log sigma, log alpha, and Y_t ####
  # Initialize the random seed
  r_seed <- r_seed_start
  for (j in 1:n_time_points){
    # Only run the initialization step if j == 1
    if (j == 1){
      # Initialize the values of logSigma, logAlpha, and R only on the first time
      for (k in 1:n_locations){
        # Set the seed
        set.seed(r_seed)
        # Create an ID variable to track things
        id_init <- k
        # Calculate the values of the parameters
        # NOTE! PLEASE READ! BETA_2 is positive in the formulas below since the parameter estimation code already makes them negative.
        # Initialize to 1.
        predicted_data_df$logSigma_pred[id_init] <- 1
        predicted_data_df$logAlpha_pred[id_init] <- 1
        # Predict the first one
        predicted_data_df$R_pred_low[id_init] <- input_param_df$mu[k]+exp(predicted_data_df$logSigma_pred[id_init])*((log(1/runif(1,0,1)))^(1/exp(predicted_data_df$logAlpha_pred[id_init])))
        predicted_data_df$R_pred_fairway[id_init] <- input_param_df$mu[k]+exp(predicted_data_df$logSigma_pred[id_init])*((log(1/runif(1,0,1)))^(1/exp(predicted_data_df$logAlpha_pred[id_init])))
        predicted_data_df$R_pred_high[id_init] <- input_param_df$mu[k]+exp(predicted_data_df$logSigma_pred[id_init])*((log(1/runif(1,0,1)))^(1/exp(predicted_data_df$logAlpha_pred[id_init])))
        # Update the random seed
        r_seed <- r_seed + 1
      }
    } else {
      # Feed through and populate the vectors
      for (k in 1:n_locations){
        # Set seed
        set.seed(r_seed)
        # Create a storage ID that will shorten the formulas
        id <- k + (j-1)*n_locations
        id_m1 <- k + (j-2)*n_locations
        # Update the parameters based on the equations on the top of p. 9. 
        # NOTE! PLEASE READ! BETA_2 and BETA_4 are positive in the formulas below since the parameter estimation code already makes them negative.
        predicted_data_df$logSigma_pred[id] <- input_param_df$beta_0[k] + (input_param_df$beta_1[k]*predicted_data_df$logSigma_pred[id_m1]) + input_param_df$beta_2[k]/(log(input_param_df$beta_3[k]*predicted_data_df$R_actual[id_m1])) + input_param_df$beta_4[k]/(log(input_param_df$beta_5[k]*predicted_data_df$R_local_max_actual[id_m1]))
        predicted_data_df$logAlpha_pred[id] <- input_param_df$gamma_0[k] + (input_param_df$gamma_1[k]*predicted_data_df$logAlpha_pred[id_m1]) + input_param_df$gamma_2[k]/(log(input_param_df$gamma_3[k]*predicted_data_df$R_actual[id_m1])) + input_param_df$gamma_4[k]/(log(input_param_df$gamma_5[k]*predicted_data_df$R_local_max_actual[id_m1]))
        # Plug into the prediction for R - low, fairway, and high
        predicted_data_df$R_pred_low[id]  <- input_param_df$mu[k]+exp(predicted_data_df$logSigma_pred[id])*(Y_low^(1/exp(predicted_data_df$logAlpha_pred[id])))
        predicted_data_df$R_pred_fairway[id]  <- input_param_df$mu[k]+exp(predicted_data_df$logSigma_pred[id])*(Y_median^(1/exp(predicted_data_df$logAlpha_pred[id])))
        predicted_data_df$R_pred_high[id]  <- input_param_df$mu[k]+exp(predicted_data_df$logSigma_pred[id])*(Y_high^(1/exp(predicted_data_df$logAlpha_pred[id])))
        # Update the random seed
        r_seed <- r_seed + 1
      }
    }
  }
  #### Prediction Function: Computing accuracy metrics and append logs ####
  # Compute the accuracy (actual - theoretical)
  predicted_data_df$resid <- predicted_data_df$R_actual - predicted_data_df$R_pred_fairway
  # Compute percent error
  predicted_data_df$percent_error <- (predicted_data_df$R_actual/predicted_data_df$R_pred_fairway - 1)*100
  # Compute the squared residuals
  predicted_data_df$squared_resid <- predicted_data_df$resid^2
  # Compute the log R values of predicted and actuals
  predicted_data_df$R_pred_low_log <- log(predicted_data_df$R_pred_low)
  predicted_data_df$R_pred_fairway_log <- log(predicted_data_df$R_pred_fairway)
  predicted_data_df$R_pred_high_log <- log(predicted_data_df$R_pred_high)
  predicted_data_df$R_actual_log <- log(predicted_data_df$R_actual)
  # Compute whether the observed value lies in the predicted range
  predicted_data_df$in_interval <- ifelse((predicted_data_df$R_pred_low < predicted_data_df$R_actual) & (predicted_data_df$R_actual < predicted_data_df$R_pred_high), 1, 0)
  # Produce a coverage table by location - exclude the first time ID
  coverage_table_by_location <- predicted_data_df %>% filter(`Time ID` != 1) %>%
    group_by(`Location ID`) %>%
    summarize(coverage_fraction = sum(in_interval)/(n_time_points-1))
  # Produce a grand coverage table
  coverage_table_grand <- predicted_data_df %>% summarize(coverage_fraction = sum(in_interval)/(n_time_points*n_locations-n_locations))
  return(list(predicted_data_df = predicted_data_df, coverage_table_by_location = coverage_table_by_location,
              coverage_table_grand = coverage_table_grand)) 
}
