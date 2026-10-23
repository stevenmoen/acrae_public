#' A function to create a table of when breaches occur for each flare, and a visualization of all breaches
#'
#' @param input_df - the input dataframe from the forecast
#' @param flare_start_point - the flare start point. For the table, all breaks AFTER are counted.
#' @param forecast_year_and_desc - the year of the forecast, for use in the plot title
#' @param location_ID_subset - locations to subset around for a second graph
#' @param save_files - save the files in the specified location? Default is 0.
#' @param n_locations - the number of locations. Don't change this.
#' @param filepath - the output filepath
#' @param burn_in_points - points excluded from the plot
#'
#' @returns - a table, and a plot
#' @export - the table and the plot to the specified filepath
#'
#' @examples - create_breach_table_and_viz(input_df = data_2022_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 494, forecast_year_and_desc = 2022)
create_breach_table_and_viz_undiff <- function(input_df, flare_start_point, forecast_year_and_desc, 
                                               location_ID_subset = c(55,56,65,66),
                                               save_files = 0,
                                               n_locations = 120,
                                               filepath = "~/Documents/GitHub/solar_flare_project/Breach_Tables_and_Visualizations/",
                                               burn_in_points = 10) {
  ##### 1. Cut down the dataset #####
  cut_dataset <- {{input_df}} %>%
    # Exclude Time 1
    filter(`Time ID` != 1) %>% 
    # Keep only important parameters
    select(`Location ID`, `Time ID`, logAlpha_pred, logSigma_pred) %>% 
    # Sort again
    arrange(`Location ID`) %>% 
    # Initialize two columns which the for loop will use, as well as a breach column
    mutate(logAlpha_rolling_min = Inf,
           breach_flag_logAlpha_rolling_min = 0,
           logSigma_rolling_max = -Inf,
           breach_flag_logSigma_rolling_max = 0)
  ##### 2. Loop through and populate #####
  for (j in 1:n_locations){
    # Create a new dataset
    one_location_dataset <- cut_dataset[cut_dataset$`Location ID` == j,]
    for (i in 2:nrow(one_location_dataset)){
      # Loop through and update the min and max values. 
      # Log Alpha - minimize
      # If the value in this step is strictly less than the minimum, update. Otherwise, no.
      one_location_dataset$logAlpha_rolling_min[i] <- ifelse(one_location_dataset$logAlpha_pred[i] < one_location_dataset$logAlpha_rolling_min[i-1], one_location_dataset$logAlpha_pred[i], one_location_dataset$logAlpha_rolling_min[i-1])
      # Use the same function to update the flag
      one_location_dataset$breach_flag_logAlpha_rolling_min[i] <- ifelse(one_location_dataset$logAlpha_pred[i] < one_location_dataset$logAlpha_rolling_min[i-1], 1, 0)
      # Log Sigma - maximize
      # If the value in this step is strictly greater than the minimum, update. Otherwise, no.
      one_location_dataset$logSigma_rolling_max[i] <- ifelse(one_location_dataset$logSigma_pred[i] > one_location_dataset$logSigma_rolling_max[i-1], one_location_dataset$logSigma_pred[i], one_location_dataset$logSigma_rolling_max[i-1])
      # Use the same function to update the flag
      one_location_dataset$breach_flag_logSigma_rolling_max[i] <- ifelse(one_location_dataset$logSigma_pred[i] > one_location_dataset$logSigma_rolling_max[i-1], 1, 0)
    }
    # Overwrite cut_dataset
    cut_dataset[cut_dataset$`Location ID` == j,] <- one_location_dataset
  }
  ##### 3. Create a logAlpha first breach table #####
  logAlpha_breach_table <- cut_dataset %>% 
    group_by(`Location ID`) %>% 
    filter(`Time ID` > flare_start_point) %>% 
    # Only include breaches AFTER the break
    filter(breach_flag_logAlpha_rolling_min > 0) %>% 
    summarize(`First Breach logAlpha` = min(`Time ID`)) 
  ##### 3a. Create a logSigma first breach table #####
  logSigma_breach_table <- cut_dataset %>% 
    group_by(`Location ID`) %>% 
    filter(`Time ID` > flare_start_point) %>% 
    # Only include breaches AFTER the break
    filter(breach_flag_logSigma_rolling_max > 0) %>% 
    summarize(`First Breach logSigma` = min(`Time ID`))
  ##### 4. Create a join dataset and create the table #####
  join_df <- data.frame(`Location ID` = seq(1,120), Flare_Start_Point = flare_start_point)
  joined_breach_table <- join_df %>% 
    left_join(y = logAlpha_breach_table, by = join_by(Location.ID == `Location ID`)) %>% 
    left_join(y = logSigma_breach_table, by = join_by(Location.ID == `Location ID`)) %>% 
    rename(`Location ID` = Location.ID, `Flare Start Point` = Flare_Start_Point)
  # The flare starts right after the 479th frame for the 2023 data. Therefore,
  # for conservatism, we assume that the flare starts at 479.
  ##### 5. Create a nice visualization #####
  breaches_over_time <- cut_dataset %>% 
    filter(`Time ID` > burn_in_points) %>% 
    group_by(`Time ID`) %>% 
    summarize(breach_flag_logAlpha_rolling_min = sum(breach_flag_logAlpha_rolling_min),
              breach_flag_logSigma_rolling_max = sum(breach_flag_logSigma_rolling_max)) %>% 
    pivot_longer(cols = breach_flag_logAlpha_rolling_min:breach_flag_logSigma_rolling_max,
                 names_to = "ID", values_to = "Breach") %>% 
    ggplot(aes(x = `Time ID`, y = Breach, color = ID, linetype = ID)) + 
    geom_line(alpha = 0.6) + 
    labs(x = "Time", y = "Breach Count", title = paste0("Breaches on ", forecast_year_and_desc),
         caption = paste0("The earliest maximum brightness value occurs at time point ", flare_start_point + 1, ". \nExcludes the first ", burn_in_points, " time points for burn-in.")) + 
    scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +
    annotate("rect", xmin=flare_start_point+1, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill="sandybrown") +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0)) + 
    scale_color_discrete(name = "Latent Process", labels = unname(TeX(c("$\\log \\ \\alpha_t$", "$\\log \\ \\sigma_t$")))) +
    scale_linetype_discrete(name = "Latent Process", labels = unname(TeX(c("$\\log \\ \\alpha_t$", "$\\log \\ \\sigma_t$"))))
  print(breaches_over_time)
  ##### 6. Create a visualization for only some locations #####
  breaches_over_time_subset <- cut_dataset %>% 
    filter(`Time ID` != 2) %>% 
    group_by(`Time ID`) %>% 
    filter(`Location ID` %in% location_ID_subset) %>% 
    summarize(breach_flag_logAlpha_rolling_min = sum(breach_flag_logAlpha_rolling_min),
              breach_flag_logSigma_rolling_max = sum(breach_flag_logSigma_rolling_max)) %>% 
    pivot_longer(cols = breach_flag_logAlpha_rolling_min:breach_flag_logSigma_rolling_max,
                 names_to = "ID", values_to = "Breach") %>% 
    ggplot(aes(x = `Time ID`, y = Breach, color = ID)) + 
    geom_line() + 
    labs(x = "Time", y = "Breach Count", title = paste0("Breaches on ", forecast_year_and_desc),
         caption = paste0("The first maximum brightness value occurs at time point ", flare_start_point + 1, ". \nExcludes the first ", burn_in_points, " time points for burn-in.")) + 
    # scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
    annotate("rect", xmin=flare_start_point+1, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill="sandybrown") +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0)) + 
    scale_color_discrete(name = "Latent Process", labels = unname(TeX(c("$\\log \\ \\alpha_t$", "$\\log \\ \\sigma_t$"))))
  # print(breaches_over_time_subset)
  # Export the files
  if (save_files == 1){
    write.csv(joined_breach_table, file = paste0(filepath, "breach_table_", forecast_year_and_desc, ".csv"))
    ggsave(filename = paste0("breaches_over_time_undiff_", forecast_year_and_desc, ".jpg"), plot = breaches_over_time, path = filepath, dpi = 600, height = 4, width = 6)
  }
  return(list(joined_breach_table = joined_breach_table, breaches_over_time = breaches_over_time, detail_breach_table = cut_dataset))
}