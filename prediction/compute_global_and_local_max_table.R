#' Compute the breaches before the global maxima
#'
#' @param detail_breach_table - a table when all the breaches occur
#' @param global_max_table - a table when the global maxima occur
#' @param local_max_table - a table when the local maxima occur in each location
#' @param burn_in_points - number of burn in points
#'
#' @returns - a table with global maxima and local maxima
#' @export
#'
#' @examples - compute_global_and_local_max_table(detail_breach_table = breach_2022_forecast_w_parameters_2022_pf_95$detail_breach_table,
#'                         global_max_table = undifferenced_2022_plot_all_locations$max_data,
#'                         local_max_table = undifferenced_2022_plot_all_locations$max_by_location_data)
compute_global_and_local_max_table <- function(detail_breach_table, global_max_table, local_max_table, burn_in_points = 10) {
  ##### 1. Clean up the detail_breach_table #####
  clean_breach_table <- {{detail_breach_table}} %>% 
    # Create an undifferenced Time ID variable
    mutate(`Undifferenced Time ID` = `Time ID` + 1,
           `Undifferenced Time ID` = as.numeric(`Undifferenced Time ID`),
           `Location ID` = as.numeric(`Location ID`)) %>% 
    # Cut down
    filter(breach_flag_logAlpha_rolling_min == 1 | breach_flag_logSigma_rolling_max == 1) %>% 
    # Select only the crucial variables
    select(`Location ID`, `Undifferenced Time ID`, breach_flag_logAlpha_rolling_min, breach_flag_logSigma_rolling_max)
  # Aggregate
  
  ##### 2. Clean up the global max table #####
  global_max_table_clean <- global_max_table %>% 
    rename(`Undifferenced Time ID` = `Time ID`) %>% 
    mutate(`Undifferenced Time ID` = as.numeric(`Undifferenced Time ID`),
           `Location ID` = as.numeric(`Location ID`)) %>% 
    # Take the first row
    filter(row_number()==1)
  
  ##### 3. Clean up the location-by-location max table #####
  local_max_table_clean <- local_max_table %>% 
    rename(`Undifferenced Time ID` = `Time ID`) %>% 
    mutate(`Undifferenced Time ID` = as.numeric(`Undifferenced Time ID`),
           `Location ID` = as.numeric(`Location ID`)) %>% 
    # Take the first row
    group_by(`Location ID`) %>% 
    filter(row_number()==1)
  
  ##### 4. Create the prediction of global max table #####
  global_max_prediction_table <- dplyr::full_join(x = clean_breach_table, y = global_max_table_clean, by = character(), suffix = c("", ".first_max")) %>% 
    # Rearrange
    select(`Location ID`, `Location ID.first_max`, `Undifferenced Time ID.first_max`, `Undifferenced Time ID`, breach_flag_logAlpha_rolling_min, breach_flag_logSigma_rolling_max) %>%
    # Cut locations before the break, but after the burn-in flag
    filter(`Undifferenced Time ID.first_max` > `Undifferenced Time ID`,
           `Undifferenced Time ID` > burn_in_points) %>% 
    # Aggregate
    group_by(`Location ID`) %>% 
    summarize(`Location ID.first_max` = mean(`Location ID.first_max`),
              `Undifferenced Time ID.first_max` = mean(`Undifferenced Time ID.first_max`),
              `Latest Detection Point` = max(`Undifferenced Time ID`),
              `logAlpha Breaks Post Burn-in` = sum(breach_flag_logAlpha_rolling_min),
              `logSigma Breaks Post Burn-in` = sum(breach_flag_logSigma_rolling_max))
  
  ##### 5. Create the prediction of local max table #####
  local_max_prediction_table <- dplyr::left_join(x = clean_breach_table, y = local_max_table_clean, by = join_by(`Location ID`), suffix = c("", ".first_max")) %>% 
    # Rearrange
    select(`Location ID`, `Undifferenced Time ID.first_max`, `Undifferenced Time ID`, breach_flag_logAlpha_rolling_min, breach_flag_logSigma_rolling_max) %>%
    # Cut locations before the break, but after the burn-in flag
    filter(`Undifferenced Time ID.first_max` > `Undifferenced Time ID`,
           `Undifferenced Time ID` > burn_in_points) %>% 
    # Aggregate
    group_by(`Location ID`) %>% 
    summarize(`Undifferenced Time ID.first_max` = mean(`Undifferenced Time ID.first_max`),
              `Latest Detection Point` = max(`Undifferenced Time ID`),
              `logAlpha Breaks Post Burn-in` = sum(breach_flag_logAlpha_rolling_min),
              `logSigma Breaks Post Burn-in` = sum(breach_flag_logSigma_rolling_max))
  
  # Return both tables
  return(list(global_max_prediction_table = global_max_prediction_table, local_max_prediction_table = local_max_prediction_table))
}