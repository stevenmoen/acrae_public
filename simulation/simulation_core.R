#' Extended simulation with the maxima. 
#' The output dataset will have all locations represented before moving to next step
#' NOTE: The log_sigma code assumes that the beta_2 and beta_4 values are negative.
#'
#' @param input_param_df - input parameter data frame. MUST contain the exact names of parameters in gen_simp
#' @param n_iter - number of iterations of the innermost for loop
#' @param n_runs - number of runs after the burn-in period
#' @param burn_in - number of burn-in runs
#' @param r_seed_start - starting point for random seed
#'
#' @return
#' @export
#'
#' @examples
extend_simul_with_max <- function(input_param_df, n_iter = 1, n_runs = 1000, burn_in = 1000, r_seed_start = 546826, use_empirical_frechet = 1, empirical_frechet_vec = r_frechet_sort, zero_beta_4_gamma_4 = 0, R_ceiling = 1e6) {
  # Set beta_4 and gamma_4 to zero to avoid "contagion" effects, where one value blows up and causes others to do the same
  if (zero_beta_4_gamma_4 == 1){
    input_param_df$beta_4 <- 0
    input_param_df$gamma_4 <- 0
  }
  # Create a total runs variable
  total_runs <- n_runs + burn_in
  # Calculate the number of locations
  n_locations <- nrow(input_param_df)
  # Print a warning if n_locations is not 120
  if(n_locations != 120){
    print("WARNING: There are not 120 locations in your input data file. This may cause issues with the code.")
  }
  # Generate a storage dataframe with 120 params times 100 iter times (1000 + 10000) rows
  simulated_params_df <- as.data.frame(matrix(0, nrow = n_locations*n_iter*(total_runs), ncol = 8))
  # Generate column names
  colnames(simulated_params_df) <- c("Run ID", "Step ID", "Location ID", "R", "logAlpha", "logSigma", "Burn-in Flag", "Previous Local Maxima")
  # Fill in the key variables
  # Run ID is the outermost variable that is the slowest to change. It stays fixed at 1 until an entire iteration is done
  # Step ID counts the step in the run. There should be 120 locations within each step
  # Location ID counts the location in each run, and within each step. It should constantly go up from 1 to 120.
  # Burn-in flag should be a bunch of 1's, followed by a bunch of 0's depending on the length of the location and the burn-in
  # simulated_params_df$`Run ID` <- rep(1:n_iter, times = total_runs*n_locations)
  # simulated_params_df$`Step ID` <- rep(rep(1:total_runs, times = n_locations), n_iter)
  simulated_params_df$`Run ID` <- rep(1:n_iter, each = total_runs*n_locations)
  simulated_params_df$`Step ID` <- rep(rep(1:total_runs, each = n_locations), n_iter)
  simulated_params_df$`Location ID` <- rep(1:n_locations, times = total_runs*n_iter)
  simulated_params_df$`Burn-in Flag` <- rep(c(rep(1, times = n_locations*burn_in), rep(0, times = n_locations*n_runs)), times = n_iter)
  # Initialize the big_mat, small_mat, and a storage vector
  big_mat <- matrix(-Inf, nrow = 14, ncol = 12)
  max_mat <- matrix(NA, nrow = 12, ncol = 10)
  sto_vec <- rep(NA, 5)
  max_vec <- rep(NA, n_locations)
  # Initialize the random seed
  r_seed <- r_seed_start
  # Put the n_iter thing on the outside
  for (i in 1:n_iter){
    # Now things are initialized, calculate the local max
    for (j in 1:total_runs){
      # Only run the initialization step if j == 1
      if (j == 1){
        # Initialize the values of logSigma, logAlpha, and R only on the first time
        for (k in 1:n_locations){
          # Set the seed
          set.seed(r_seed)
          # Create an ID variable to track things
          id_init <- k + (i-1)*total_runs
          # Calculate the values of the parameters
          # NOTE! PLEASE READ! BETA_2 is positive in the formulas below since the parameter estimation code already makes them negative.
          # Try initializing to 1.
          simulated_params_df$logSigma[id_init] <- 1
          simulated_params_df$logAlpha[id_init] <- 1
          # Draw a random number for
          if (use_empirical_frechet == 1){
            # Select a position to draw from (fp = frechet position)
            fp <- round(runif(1, min = 1, max = length(empirical_frechet_vec)))
            simulated_params_df$R[id_init] <- input_param_df$mu[k]+exp(simulated_params_df$logSigma[id_init])*((empirical_frechet_vec[fp])^(1/exp(simulated_params_df$logAlpha[id_init])))
            # simulated_params_df$R[id_init] <- 
          } else {
            # 1/uniform option
            simulated_params_df$R[id_init] <- input_param_df$mu[k]+exp(simulated_params_df$logSigma[id_init])*((log(1/runif(1,0,1)))^(1/exp(simulated_params_df$logAlpha[id_init])))
          }
          # Update the random seed
          r_seed <- r_seed + 1
        }
      } else {
        # Put the data into a matrix
        big_mat[2:13, 2:11] <- matrix(simulated_params_df$R[(1 + (j-2)*n_locations + (i-1)*total_runs): (n_locations + (j-2)*n_locations + (i-1)*total_runs)], nrow = 12, ncol = 10, byrow = TRUE) 
        # Loop through and populate the max_mat
        for (i_ in 2:13){
          for (j_ in 2:11){
            # Updated on 2/19 - this takes the maxima around in a square, starting with lower left, and moving clockwise.
            sto_vec <- c(big_mat[i_-1, j_-1], big_mat[i_-1, j_], big_mat[i_-1, j_+1], big_mat[i_, j_+1],
                         big_mat[i_+1, j_+1], big_mat[i_+1, j_], big_mat[i_+1, j_-1], big_mat[i_, j_-1])
            # Put the max into the max_mat
            max_mat[i_-1, j_-1] <- max(sto_vec)
          }
        }
        # Now we need to flatten this back into a vector
        max_vec <- c(t(max_mat))
        # Put this in the data frame
        simulated_params_df$`Previous Local Maxima`[(1 + (j-1)*n_locations + (i-1)*total_runs):(n_locations + (j-1)*n_locations + (i-1)*total_runs)] <- max_vec
        # Feed through and populate the vectors
        for (k in 1:n_locations){
          # Set seed
          set.seed(r_seed)
          # Create a storage ID that will shorten the formulas
          id <- k + (j-1)*n_locations + (i-1)*total_runs
          id_m1 <- k + (j-2)*n_locations + (i-1)*total_runs
          # Update the parameters based on the equations on the top of p. 9. 
          # NOTE! PLEASE READ! BETA_2 and BETA_4 are positive in the formulas below since the parameter estimation code already makes them negative.
          simulated_params_df$logSigma[id] <- input_param_df$beta_0[k] + (input_param_df$beta_1[k]*simulated_params_df$logSigma[id_m1]) + input_param_df$beta_2[k]/(log(input_param_df$beta_3[k]*simulated_params_df$R[id_m1])) + input_param_df$beta_4[k]/(log(input_param_df$beta_5[k]*max_vec[k]))
          # simulated_params_df$logSigma[id] <- input_param_df$beta_0[k] + (input_param_df$beta_1[k]*simulated_params_df$logSigma[id_m1]) - input_param_df$beta_2[k]/(log(input_param_df$beta_3[k]*simulated_params_df$R[id_m1])) - input_param_df$beta_4[k]/(log(input_param_df$beta_5[k]*max_vec[k]))
          simulated_params_df$logAlpha[id] <- input_param_df$gamma_0[k] + (input_param_df$gamma_1[k]*simulated_params_df$logAlpha[id_m1]) + input_param_df$gamma_2[k]/(log(input_param_df$gamma_3[k]*simulated_params_df$R[id_m1])) + input_param_df$gamma_4[k]/(log(input_param_df$gamma_5[k]*max_vec[k]))
          # Frechet option
          if (use_empirical_frechet == 1){
            # Select a position to draw from (fp = frechet position)
            fp <- round(runif(1, min = 1, max = length(empirical_frechet_vec)))
            # Ceiling the value
            simulated_params_df$R[id]  <- min(input_param_df$mu[k]+exp(simulated_params_df$logSigma[id])*((empirical_frechet_vec[fp])^(1/exp(simulated_params_df$logAlpha[id]))), R_ceiling)
            # simulated_params_df$R[id]  <- input_param_df$mu[k]+exp(simulated_params_df$logSigma[id])*((empirical_frechet_vec[fp])^(1/exp(simulated_params_df$logAlpha[id])))
          } else {
            # 1/uniform option
            simulated_params_df$R[id]  <- input_param_df$mu[k]+exp(simulated_params_df$logSigma[id])*((log(1/runif(1,0,1)))^(1/exp(simulated_params_df$logAlpha[id])))
          }
          r_seed <- r_seed + 1
        }
      }
    }
  }
  return(simulated_params_df) 
}