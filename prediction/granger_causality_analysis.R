#' Study the relationship between Avg. Brightness, logSigma, and logAlpha using a Granger Causality analysis
#'
#' @param undifferenced_plot_data - averaged plot data
#' @param predicted_data_df - the dataframe with predictions from the 2011 dataset
#' @param burn_in_points - defaults to 10
#' @param n_locations - number of locations
#' @param n_lags - the number of lags to consider.
#'
#' @returns
#' @export
#'
#' @examples - granger_causality_analysis(undifferenced_plot_data = undifferenced_2022_plot_all_locations$plot_data,
#'                           predicted_data_df = data_2022_forecast_w_parameters_2011_new_95$predicted_data_df)
granger_causality_analysis <- function(undifferenced_plot_data, predicted_data_df, burn_in_points = 10, n_locations = 120, n_lags = 1) {
  # Initalize a storage dataframe
  GC_p_value_df <- data.frame(response = rep(c("Avg. Brightness", "Avg. Brightness", "logAlpha", "logSigma"), n_locations),
                              predictor = rep(c("logAlpha", "logSigma", "Avg. Brightness", "Avg. Brightness"), n_locations),
                              lags_considered = rep(n_lags, 4*n_locations),
                              `Location ID` = rep(1:120, each = 4),
                              p_values = rep(0, 4*n_locations))
  # Separate the plot data into a different location
  for (i in 1:120){
    ##### 1. Subset the actual data to only one location #####
    one_location_undifferenced_data <-  {{undifferenced_plot_data}} %>% 
      filter(`Location ID` == i) %>% 
      rename(`Undifferenced Time ID` = `Time ID`) %>% 
      mutate(`Undifferenced Time ID` = as.numeric(`Undifferenced Time ID`),
             `Location ID` = as.numeric(`Location ID`))
    
    ##### 2. Subset the prediction to only one location #####
    one_location_latent_predictions <- {{predicted_data_df}}  %>% 
      filter(`Location ID` == i) %>% 
      # Add the undifferenced time ID
      mutate(`Undifferenced Time ID` = `Time ID` + 1,
             `Undifferenced Time ID` = as.numeric(`Undifferenced Time ID`),
             `Location ID` = as.numeric(`Location ID`)) %>% 
      # Select only what's necessary
      select(`Undifferenced Time ID`, `Location ID`, logAlpha_pred, logSigma_pred)
    
    ##### 3. Join the actual data to the predictions #####
    joined_data_for_modeling <- dplyr::full_join(x = one_location_undifferenced_data, y = one_location_latent_predictions,
                                                 by = join_by(`Undifferenced Time ID`, `Location ID`)) %>% 
      # Shorten the dataset by the burn-in period
      filter(`Undifferenced Time ID` > burn_in_points)
    
    ##### 4. Do Granger Causality tests based on an AR(1) model with Avg. Brightness as the response #####
    ar1_model_base <- lm(joined_data_for_modeling$`Avg. Brightness` ~ lag(joined_data_for_modeling$`Avg. Brightness`, n = 1))
    ar1_model_with_logAlpha <- lm(joined_data_for_modeling$`Avg. Brightness` ~ lag(joined_data_for_modeling$`Avg. Brightness`, n = 1) + 
                                    lag(joined_data_for_modeling$logAlpha_pred, n = 1))
    ar1_model_with_logSigma <- lm(joined_data_for_modeling$`Avg. Brightness` ~ lag(joined_data_for_modeling$`Avg. Brightness`, n = 1) + 
                                    lag(joined_data_for_modeling$logSigma_pred, n = 1))
    # Do an F-test for each one
    logAlpha_gc_anova <- anova(ar1_model_base, ar1_model_with_logAlpha)
    logSigma_gc_anova <- anova(ar1_model_base, ar1_model_with_logSigma)
    # Extract the p-value
    logAlpha_gc_p_value <- logAlpha_gc_anova$`Pr(>F)`[2]
    logSigma_gc_p_value <- logSigma_gc_anova$`Pr(>F)`[2]
    # Store the p-values
    GC_p_value_df$p_values[(4*(i-1) + 1)] <- logAlpha_gc_p_value
    GC_p_value_df$p_values[(4*(i-1) + 2)] <- logSigma_gc_p_value
    
    ##### 5. Do reverse Granger Causality tests based on an AR(1) model with logAlpha and logSigma as the responses #####
    ar1_model_logAlpha <- lm(joined_data_for_modeling$logAlpha_pred ~ lag(joined_data_for_modeling$logAlpha_pred, n = 1))
    ar1_model_logSigma <- lm(joined_data_for_modeling$logSigma_pred ~ lag(joined_data_for_modeling$logSigma_pred, n = 1))
    ar1_model_logAlpha_w_lagged_actuals <- lm(joined_data_for_modeling$logAlpha_pred ~ lag(joined_data_for_modeling$logAlpha_pred, n = 1) +
                                                lag(joined_data_for_modeling$`Avg. Brightness`, n = 1))
    ar1_model_logSigma_w_lagged_actuals <- lm(joined_data_for_modeling$logSigma_pred ~ lag(joined_data_for_modeling$logSigma_pred, n = 1) +
                                                lag(joined_data_for_modeling$`Avg. Brightness`, n = 1))
    # Do an F-test for each one
    reverse_logAlpha_gc_anova <- anova(ar1_model_logAlpha, ar1_model_logAlpha_w_lagged_actuals)
    reverse_logSigma_gc_anova <- anova(ar1_model_logSigma, ar1_model_logSigma_w_lagged_actuals)
    # Extract the p-value
    reverse_logAlpha_gc_p_value <- reverse_logAlpha_gc_anova$`Pr(>F)`[2]
    reverse_logSigma_gc_p_value <- reverse_logSigma_gc_anova$`Pr(>F)`[2]
    # Store the p-values
    GC_p_value_df$p_values[(4*(i-1) + 3)] <- reverse_logAlpha_gc_p_value
    GC_p_value_df$p_values[(4*(i-1) + 4)] <- reverse_logSigma_gc_p_value
  }
  # Create a combined column
  GC_p_value_df$response_v_predictor = paste0(GC_p_value_df$response, "_y_", GC_p_value_df$predictor, "_x")
  return(GC_p_value_df)
}