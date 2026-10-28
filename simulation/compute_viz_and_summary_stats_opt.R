#' This code creates an updated histogram, qq-plot, and summary statistics table, choosing an optimal period to plot.
#'
#' @param simulated_data - the simulated data you'd like to plot.
#' @param burn_in - the length of the burn-in in the code
#' @param original_data - the original data to compare to. Don't change this.
#' @param output_filepath - the filepath where the output is stored.
#'
#' @returns - two plots printed to the console and the dataset needed to prepare them
#' @export - exports the optimal qq-plot and histogram
#'
#' @examples
compute_viz_and_summary_stats_opt <- function(simulated_data, burn_in, original_data = data_old,
                                              output_filepath = "~/Documents/GitHub/solar_flare_project/Edited_Figures") {
  ##### 1. Compute a cleaned version of the original data ######
  original_data_clean <- {{original_data}} %>% 
    # Add a time column
    mutate(Time = seq(1,299,1)) %>%
    # Select relevant columns
    dplyr::select(Time, V1, V12, V33, V34, V44, V45) %>% # NEW - six locations
    # Rename
    rename(NSF.r1c1 = V1, NSF.r2c2 = V12, SF1.r4c3 = V33, SF1.r4c4 = V34, SF2.r5c4 = V44, SF2.r5c5 = V45) %>% # NEW - six locations
    # Pivot longer
    pivot_longer(cols = `NSF.r1c1`:`SF2.r5c5`, 
                 names_to = "Location",
                 values_to = "Observed Brightness") %>% 
    # Create a log brightness value
    mutate(`Observed Log Brightness` = log(`Observed Brightness`)) %>% 
    # Sort
    select(Location, `Observed Log Brightness`) %>% 
    group_by(Location) %>% 
    arrange(Location, `Observed Log Brightness`) %>% 
    ungroup() %>% 
    # Create a dummy join key
    mutate(join_key = rep(seq(1, 299), 6)) %>% # NEW - six locations
    # arrange(Location, Time) %>% 
    select(Location, join_key, `Observed Log Brightness`)
  
  ##### 2. Pre-clean the simulated dataset #####
  simulated_data_clean <- {{simulated_data}} %>%
    # Cut to specific locations
    filter(`Location ID` %in% c(1, 12, 33, 34, 44, 45)) %>% # NEW - six locations
    # Create a location column, as well as a Time column
    mutate(Location = case_when(`Location ID` == 1 ~ "NSF.r1c1", # NEW - six locations
                                `Location ID` == 12 ~ "NSF.r2c2",
                                `Location ID` == 33 ~ "SF1.r4c3",
                                `Location ID` == 34 ~ "SF1.r4c4",
                                `Location ID` == 44 ~ "SF2.r5c4",
                                `Location ID` == 45 ~ "SF2.r5c5")) %>% # OLD
    rename(`Simulated Brightness` = R,
           Time = `Step ID`) %>% 
    # Create a Simulated Log Brightness column
    mutate(`Simulated Log Brightness` = log(`Simulated Brightness`)) %>% 
    dplyr::select(Location, Time, `Simulated Log Brightness`) %>%
    arrange(Location, Time)
  
  ##### 3. Join the datasets many times, and select an optimal dataset #####
  # Compute the max i. Should be burn_in + 1
  max_i = burn_in + 1
  n_rows_original_data = nrow(original_data)
  # Create a storage_dataset
  fit_dataset <- data.frame(SSE = rep(0, max_i),
                            SST = rep(0, max_i),
                            R_2 = rep(0, max_i),
                            start_time = rep(0, max_i),
                            end_time = rep(0, max_i))
  # Join the two
  for (i in 1:max_i){
    # Cut down the dataset
    fit_dataset[i,] <- simulated_data_clean %>% 
      # Select data after i, but before 1000 + 300 
      filter(Time >= i,
             Time < (n_rows_original_data + i)) %>% 
      # Sort
      select(Location, `Simulated Log Brightness`) %>% 
      group_by(Location) %>% 
      arrange(Location, `Simulated Log Brightness`) %>% 
      ungroup() %>% 
      # Create a dummy join key
      mutate(join_key = rep(seq(1, 299), 6)) %>% # NEW - six locations
      # Join the data
      left_join(original_data_clean, by = join_by(Location, join_key)) %>% 
      # Compute the SSE
      mutate(SSE_component = (`Observed Log Brightness`- `Simulated Log Brightness`)^2,
             SST_component = (`Observed Log Brightness`- mean(`Observed Log Brightness`))^2) %>% 
      # Compute SSE, SST, and R^2
      summarize(SSE = sum(SSE_component),
                SST = sum(SST_component)) %>% 
      mutate(R_2 = (SST-SSE)/SST) %>%
      # Append two variables
      mutate(start_time = i,
             end_time = n_rows_original_data + i - 1)
  }
  
  # Plot the r-squared
  R_2_plot <- fit_dataset %>% 
    ggplot(aes(x = start_time, y = R_2)) +
    labs(x = "Start Time", y = "R Squared Between Observed and Simulated Data") +
    geom_line()
  # print(fit_dataset)
  # print(R_2_plot)
  # Find the optimal point
  best_row <- which.max(fit_dataset$R_2)

    # Extract the best start_time and end_time
  best_start_time <- fit_dataset$start_time[best_row]
  best_end_time <- fit_dataset$end_time[best_row]
  
  ##### 4. Refit the optimal dataset and plot #####
  
  # Refit the dataset
  wide_optimal_dataset <- simulated_data_clean %>% 
    # Use the values above
    filter(Time >= best_start_time,
           Time < (best_end_time + 1)) %>% 
    # Sort
    select(Location, `Simulated Log Brightness`) %>% 
    group_by(Location) %>% 
    arrange(Location, `Simulated Log Brightness`) %>% 
    ungroup() %>% 
    # Create a dummy join key
    mutate(join_key = rep(seq(1, 299), 6)) %>% # NEW - six locations
    # Join the data
    left_join(original_data_clean, by = join_by(Location, join_key)) %>% 
    # Cut out the join key
    select(-join_key)
  
  # Pivot longer
  optimal_dataset <- wide_optimal_dataset %>%
    pivot_longer(cols = `Simulated Log Brightness`:`Observed Log Brightness`, 
                 names_to = "Description",
                 values_to = "Brightness")
  # Create a qqplot
  qq_plot <- wide_optimal_dataset %>% 
    ggplot(aes(x = `Observed Log Brightness`, y = `Simulated Log Brightness`)) +
    geom_point() +
    geom_abline() +
    theme(plot.caption = element_text(hjust=0)) + 
    ylim(4,13)+
    facet_wrap(~Location, nrow = 2, ncol = 3) + # NEW
    # facet_wrap(~Location, nrow = 1, ncol = 3) + # OLD
    labs(caption = paste0("1. Both the simulated and observed data contain 299 data points.\n2. The dataset was chosen by comparing the real data to ", scales::comma(burn_in + 1), " candidate simulations and choosing the best one.\nThis shows that it is possible to get a simulation to align with the real data.\n3. 1,000 unit Fréchet random variables were drawn using random seed 227615 to induce randomness in the simulation.\n4. For numerical stability, the maximum value in the simulation was 1,000,000."))
  
  # Print the plot
  print(qq_plot)
  # Save the plot
  # ggsave(filename="empirical_qq_plot_opt.jpg", plot=qq_plot, path = output_filepath, dpi = 600)
  
  # Create a histogram
  histogram_plot <- optimal_dataset %>% 
    ggplot(aes(x = Brightness, fill = Description, pattern = Description)) +
    # Add lines
    geom_histogram(alpha = 0.5, color = "grey50", position = "identity") +
    facet_wrap(~Location, nrow = 3, ncol = 2) + # NEW
    # Fix the theme
    theme_bw() +
    theme(plot.caption = element_text(hjust=0)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    # Add labels
    labs(x = "Log Brightness",
         y = "Count",
         caption = paste0("1. Both the simulated and observed data contain 299 data points.\n2. The dataset was chosen by comparing the real data to ", scales::comma(burn_in + 1), " candidate simulations and choosing the best one.\nThis shows that it is possible to get a simulation to align with the real data.\n3. 1,000 unit Fréchet random variables were drawn using random seed 227615 to induce randomness in the simulation.\n4. For numerical stability, the maximum value in the simulation was 1,000,000.")) +
    # Change the legend labels
    scale_fill_discrete(labels=c('Observed Data', 'Simulated Data')) + 
    scale_fill_manual(values = c("orange", "blue"))
  # Add specific colors and linetypes
  print(histogram_plot)
  # Save the plot
  # ggsave(filename="revised_fig6_histogram_opt.jpg", plot=histogram_plot, path = output_filepath, dpi = 600)
  # Create an empirical q-q plot
  # visualization_dataset
  # Look at summary statistics
  optimal_dataset %>% 
    group_by(Location, Description) %>% 
    summarize(mean_brightness = mean(Brightness),
              SD_brightness = sd(Brightness),
              q1_brightness = quantile(Brightness, probs = 0.25),
              median_brightness = median(Brightness),
              q3_brightness = quantile(Brightness, probs = 0.75),
              min_brightness = min(Brightness),
              max_brightness = max(Brightness)) %>% print()
  return(list(optimal_dataset = optimal_dataset, wide_optimal_dataset = wide_optimal_dataset))
}