# This code reproduces Tables 3 and 4

# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following paths
your_path_to_parameters = "~/Documents/GitHub/acrae_public/parameters/"
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"
your_path_to_prediction = "~/Documents/GitHub/acrae_public/prediction/"
your_path_to_data_cleaning = "~/Documents/GitHub/acrae_public/data_cleaning/"
your_path_to_2017_data = "~/Cleaned_Flare_Data/2017/"
your_path_to_2022_data = "~/Cleaned_Flare_Data/2022/"
your_path_to_2023_data = "~/Cleaned_Flare_Data/2023/"

# Load in libraries
library(VGAM)

# The parameters should be saved in these subfolders
folder_file_2011_new_parameter = "Solar_Flare_2011_new_data/parameter3_clean_debug_flag_0_2024-08-30.csv"

# Read in parameters
parameters_2011_new <- read.csv(file = paste0(your_path_to_parameters, folder_file_2011_new_parameter), header = TRUE) 

# The data should be saved in these subfolders
folder_file_2017_data = "Processed_Data/2017_Data_Para_Input_Files_New/max_df_2017_new_2026-02-16.rds"
folder_file_2022_data = "Processed_Data/2022_Data_Para_Input_Files_New/max_df_2022_new_2024-08-29.rds"
folder_file_2023_data = "Processed_Data/2023_Data_Para_Input_Files_New/max_df_2023_new_2024-08-30.rds"

# Whole period data
data_2017 <- readRDS(file = paste0(your_path_to_data, folder_file_2017_data))
data_2022 <- readRDS(file = paste0(your_path_to_data, folder_file_2022_data))
data_2023 <- readRDS(file = paste0(your_path_to_data, folder_file_2023_data))

# Load in data cleaning code for undifferenced data
source(paste0(your_path_to_data_cleaning, "clean_undifferenced_data.R"))

# Note that these files can be large, particularly the new_data file. Run code with caution.

# Read in the 2017 data
file_name_2017 = "flare_2017.csv"
new_data = read.csv(file = paste0(your_path_to_2017_data, file_name_2017), header = F)
undifferenced_2017_data <- trim_df(input_df = new_data, cut_end_rows = 0, cut_end_columns = 1, 
                                   bottom_row_cut_flag = 1, bottom_row_num = 227,
                                   intended_post_trim_rows = 226, intended_post_trim_columns = 428,
                                   output_rows = 216, output_columns = 420, total_images = 900,
                                   post_process_target_rows = 12, post_process_target_columns = 10, print_mat_dim_flag = 1)

# Read in the 2022 data
file_name_2022 = "flare_2022.csv"
new_data = read.csv(file = paste0(your_path_to_2022_data, file_name_2022), header = F)
undifferenced_2022_data <- trim_df(input_df = new_data, cut_end_rows = 0, cut_end_columns = 1, 
                                   bottom_row_cut_flag = 0, bottom_row_num = 262,
                                   intended_post_trim_rows = 262, intended_post_trim_columns = 168,
                                   output_rows = 252, output_columns = 160, total_images = 900,
                                   post_process_target_rows = 12, post_process_target_columns = 10, print_mat_dim_flag = 1)

# Read in the 2023 data
file_name_2023 = "flare_2023.csv"
new_data = read.csv(file = paste0(your_path_to_2023_data, file_name_2023), header = F)
undifferenced_2023_data <- trim_df(input_df = new_data, cut_end_rows = 0, cut_end_columns = 1, 
                                   bottom_row_cut_flag = 0, bottom_row_num = 250,
                                   intended_post_trim_rows = 250, intended_post_trim_columns = 94,
                                   output_rows = 240, output_columns = 90, total_images = 900,
                                   post_process_target_rows = 12, post_process_target_columns = 10, print_mat_dim_flag = 1)

# Remove new_data from memory
rm(new_data)

# Load in function
source(paste0(your_path_to_data_cleaning, "clean_and_plot_undifferenced_data.R"))

##### Table 3 #####

# Many of the arguments are unnecessary. Don't change them. You are trying to find the earliest point with the highest average brightness
undifferenced_2017_plot_all_locations <- clean_and_plot_undifferenced_data(undifferenced_df = undifferenced_2017_data, flare_start_point = 234, detection_point = 493, forecast_year_and_desc = 2017, center_location_flag = 0,
                                                                           time_points = 900, output_rows = 216, output_columns = 420)
undifferenced_2017_plot_all_locations$max_data # This gives you the time point and location of the highest average brightness
# In location 83, at time point 234 on the undifferenced scale was the highest average brightness.
# After differencing, this gives us time point 233, which is seen in Table 3
# As for the location, r9c3 comes from adding 1 to the tens place to get the row number, and extracting the column number from the units place
# (The data is read in row-wise into the matrices)

undifferenced_2022_plot_all_locations <- clean_and_plot_undifferenced_data(undifferenced_df = undifferenced_2022_data, flare_start_point = 494, detection_point = 550, forecast_year_and_desc = 2022, center_location_flag = 0,
                                                                           time_points = 900, output_rows = 252, output_columns = 160)
undifferenced_2022_plot_all_locations$max_data # This gives you the time point and location of the highest average brightness
# After differencing, this gives us time point 551, which is seen in Table 3
# As for the location, r7c7 comes from adding 1 to the tens place to get the row number, and extracting the column number from the units place

undifferenced_2023_plot_all_locations <- clean_and_plot_undifferenced_data(undifferenced_df = undifferenced_2023_data, flare_start_point = 479, detection_point = 493, forecast_year_and_desc = 2023, center_location_flag = 0,
                                                                           time_points = 900, output_rows = 240, output_columns = 90)
undifferenced_2023_plot_all_locations$max_data # This gives you the time point and location of the highest average brightness
# After differencing, this gives us time point 495, which is seen in Table 3. 
# While there are many locations with the highest value of avg. brightness, these are the earliest time points.
# As for the location, r7c4 and r8c4 comes from adding 1 to the tens place to get the row number, and extracting the column number from the units place

# Next, we need to run three forecasts to get the movement of the tail indices, log sigma and log alpha. These don't depend on the confidence level
source(paste0(your_path_to_prediction, "prediction_core.R"))

# Work on 11/7/2024
# runif(1, 0, 1000000)
# [1] 227615.3
# Generate 1,000 random Fréchet random variables
set.seed(227615)
r_frechet_draws <- rfrechet(n = 1000, shape = 1)
r_frechet_sort <- sort(r_frechet_draws) # Sort the vector

# 2017 whole period data
## 2011 new parameters
data_2017_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2017, input_param_df = parameters_2011_new, alpha = 0.05)

# 2022 whole period data
## 2011 new parameters
data_2022_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2022, input_param_df = parameters_2011_new, alpha = 0.05)

# 2023 whole period data
## 2011 new parameters
data_2023_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2023, input_param_df = parameters_2011_new, alpha = 0.05)

# Load in function to compute breaches, which sees if the log Alpha or log Sigma goes below its rolling min or max
source(paste0(your_path_to_prediction, "create_breach_table_and_viz_undiff.R"))

# Run the function
breach_2017_forecast_w_parameters_2011_new_95 <- create_breach_table_and_viz_undiff(input_df = data_2017_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 232, forecast_year_and_desc = "2017 Data with 2011 New Parameters")
breach_2022_forecast_w_parameters_2011_new_95 <- create_breach_table_and_viz_undiff(input_df = data_2022_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 550, forecast_year_and_desc = "2022 Data with 2011 New Parameters")
breach_2023_forecast_w_parameters_2011_new_95 <- create_breach_table_and_viz_undiff(input_df = data_2023_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 494, forecast_year_and_desc = "2023 Data with 2011 New Parameters")

# Load in function
source(paste0(your_path_to_prediction, "compute_global_and_local_max_table.R"))

breach_times_2017 = compute_global_and_local_max_table(detail_breach_table = breach_2017_forecast_w_parameters_2011_new_95$detail_breach_table,
                                   global_max_table = undifferenced_2017_plot_all_locations$max_data,
                                   local_max_table = undifferenced_2017_plot_all_locations$max_by_location_data)
breach_times_2022 = compute_global_and_local_max_table(detail_breach_table = breach_2022_forecast_w_parameters_2011_new_95$detail_breach_table,
                                   global_max_table = undifferenced_2022_plot_all_locations$max_data,
                                   local_max_table = undifferenced_2022_plot_all_locations$max_by_location_data)
breach_times_2023 = compute_global_and_local_max_table(detail_breach_table = breach_2023_forecast_w_parameters_2011_new_95$detail_breach_table,
                                   global_max_table = undifferenced_2023_plot_all_locations$max_data,
                                   local_max_table = undifferenced_2023_plot_all_locations$max_by_location_data)

# These tables give you third rows of Table 3. Note that these are computed on the non-differenced scale, so you'll need to subtract 1 from each
breach_times_2017$local_max_prediction_table |> filter(`Location ID` == 83)
breach_times_2022$local_max_prediction_table |> filter(`Location ID` == 67)
breach_times_2023$local_max_prediction_table |> filter(`Location ID` %in% c(64,74))

##### Table 4 #####
source(paste0(your_path_to_prediction, "granger_causality_analysis.R"))

# 2017 column
gc_p_value_table_2017 = granger_causality_analysis(undifferenced_plot_data = undifferenced_2017_plot_all_locations$plot_data,
                                                   predicted_data_df = data_2017_forecast_w_parameters_2011_new_95$predicted_data_df)
gc_p_value_table_2017 %>% 
  mutate(reject_null = ifelse(p_values < 0.05, 1, 0)) %>% 
  group_by(response_v_predictor) %>% 
  summarize(reject_null = sum(reject_null))

# 2022 column
gc_p_value_table_2022 = granger_causality_analysis(undifferenced_plot_data = undifferenced_2022_plot_all_locations$plot_data,
                                                   predicted_data_df = data_2022_forecast_w_parameters_2011_new_95$predicted_data_df)
gc_p_value_table_2022 %>% 
  mutate(reject_null = ifelse(p_values < 0.05, 1, 0)) %>% 
  group_by(response_v_predictor) %>% 
  summarize(reject_null = sum(reject_null))

# 2023 column
gc_p_value_table_2023 = granger_causality_analysis(undifferenced_plot_data = undifferenced_2023_plot_all_locations$plot_data,
                                                   predicted_data_df = data_2023_forecast_w_parameters_2011_new_95$predicted_data_df)
gc_p_value_table_2023 %>% 
  mutate(reject_null = ifelse(p_values < 0.05, 1, 0)) %>% 
  group_by(response_v_predictor) %>% 
  summarize(reject_null = sum(reject_null))