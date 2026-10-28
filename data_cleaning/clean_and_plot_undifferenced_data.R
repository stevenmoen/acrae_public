#' A function to clean and plot the data
#'
#' @param undifferenced_df - the undifferenced data frame
#' @param flare_start_point - the last point before the flare starts.
#' @param detection_point - the first point the flare is detectable by latent processes
#' @param forecast_year_and_desc - the description of the flare for use in the title. "2022" or "2023" will suffice
#' @param time_points - the number of time points 
#' @param output_rows - the number of output rows from the trim_df function originally used to pull the data.
#' @param output_columns - the number of output columns from the trim_df function originally used to pull the data.
#' @param post_process_target_rows - the number of target rows. Default is 12. Don't change this.
#' @param post_process_target_columns  - the number of target columns. Default is 10. Don't change this.
#' @param center_location_flag - a flag to only look at the central locations
#'
#' @returns
#' @export
#'
#' @examples
clean_and_plot_undifferenced_data <- function(undifferenced_df, flare_start_point, detection_point, forecast_year_and_desc, center_location_flag = 1,
                                              time_points, output_rows, output_columns, post_process_target_rows = 12, post_process_target_columns = 10) {
  ##### 0. Data Cleaning #####
  undifferenced_vec <- {{undifferenced_df}} %>% t() %>% as.vector()
  # The key point is that this really does read in things row-wise, which is how our model works. Row-wise.
  # Aligns with previous work
  # This adds a Time ID that we'll need for plotting
  time_id_vec <- rep(1:time_points, each = output_rows*output_columns)
  # The first layer is to rep the location ID nine times. Then, we rep that 20 times. This has to do with the dimensions. There are 9 entries in each column that feed into one location, and 20 rows.
  location_id_1_10 <- rep(rep(1:10, each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  # Repeat for other 110 locations
  location_id_11_20 <-   rep(rep(11:20,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_21_30 <-   rep(rep(21:30,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_31_40 <-   rep(rep(31:40,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_41_50 <-   rep(rep(41:50,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_51_60 <-   rep(rep(51:60,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_61_70 <-   rep(rep(61:70,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_71_80 <-   rep(rep(71:80,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_81_90 <-   rep(rep(81:90,   each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_91_100 <-  rep(rep(91:100,  each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_101_110 <- rep(rep(101:110, each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  location_id_111_120 <- rep(rep(111:120, each = output_columns/post_process_target_columns), output_rows/post_process_target_rows)
  # Combine all of these into one massive vector, and then rep it 900 times
  location_id_vec <- rep(c(location_id_1_10, location_id_11_20, location_id_21_30, location_id_31_40,
                           location_id_41_50, location_id_51_60, location_id_61_70, location_id_71_80, 
                           location_id_81_90, location_id_91_100, location_id_101_110, location_id_111_120), time_points)
  # Combine into a dataframe
  # The first column is the `Time ID` variable
  # Now combine into a dataframe
  undifferenced_df <- data.frame(time_id_vec, location_id_vec, undifferenced_vec)
  ##### 1. Plot the Data #####
  plot_data <- undifferenced_df %>% 
    # Convert column to factor
    mutate(location_id_vec = as.factor(location_id_vec)) %>% 
    # Rename
    rename(`Time ID` = time_id_vec,
           `Location ID` = location_id_vec,
           `Brightness` = undifferenced_vec) %>% 
    # Filter to only one location
    filter(case_when(center_location_flag == 1 ~ `Location ID` %in% c(55,56,65,66),
                     TRUE ~ `Location ID` %in% seq(1,120))) %>% 
    # Average across the locations
    group_by(`Time ID`, `Location ID`) %>% 
    summarize(`Avg. Brightness` = mean(`Brightness`),
              `Max Brightness` = max(`Brightness`))
  # Feed in the plot data
  output_plot <- plot_data %>% 
    ggplot(aes(x = `Time ID`, y = `Avg. Brightness`, color = `Location ID`)) +
    # ggplot(aes(x = `Time ID`, y = `Max Brightness`, color = `Location ID`)) +
    geom_point()+
    geom_vline(xintercept = detection_point, col = "red", lwd = 1)+
    annotate("rect", xmin=flare_start_point+1, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill="sandybrown")+
    labs(x = "Time", y = "Brightness Averaged by Location", color = "Locations",
         title = paste0("Plots of Undifferenced Brightness for ", forecast_year_and_desc),
         caption = paste0("The flare begins after time point ", flare_start_point, "\nThe red line at time point ", detection_point, " refers to the best prediction of the solar flare from these locations.")) + 
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
  # Print the plot
  # print(output_plot)
  ##### 2. Compute the max by region #####
  max_by_location_data <- plot_data %>% 
    ungroup() %>% 
    # group_by(`Location ID`) %>% 
    slice_max(n = 1, order_by = `Avg. Brightness`, by = `Location ID`)
  ##### 3. Compute the global maxima #####
  max_data <- plot_data %>% 
    ungroup() %>% 
    # group_by(`Location ID`) %>% 
    slice_max(n = 1, order_by = `Avg. Brightness`)
  ##### 4. Return the plot and datasets #####
  return(list(output_plot = output_plot, plot_data = plot_data, max_by_location_data = max_by_location_data, max_data = max_data))
}