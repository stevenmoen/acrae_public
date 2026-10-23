# This code reproduces Figure 2, Figure S38, and Figure S39

# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following paths
your_path_to_parameters = "~/Documents/GitHub/acrae_public/parameters/"
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"
your_path_to_prediction = "~/Documents/GitHub/acrae_public/prediction/"

# Load in libraries
library(tidyverse)
# library(magrittr)
library(kableExtra)
# library(tidyverse)
library(VGAM)
# library(formattable)
# library(magick)
# library(webshot2)
# library(tidyquant)
library(latex2exp)

# The parameters should be saved in these subfolders
# folder_file_2011_original_fs_parameter = "Original_Parameters_from_Supplement/Original_Parameters_from_Supplement.csv"
# folder_file_2011_old_parameter = "Run\ from\ 7.28.2022/parameter3_clean_debug_flag_0_2024-02-19.csv"
# folder_file_2011_old_parameter_r2 = "Solar_Flare_2011_new_data/parameter3_clean_debug_flag_0_2024-06-10.csv"
folder_file_2011_new_parameter = "Solar_Flare_2011_new_data/parameter3_clean_debug_flag_0_2024-08-30.csv"
# folder_file_2022_parameter = "Solar_Flare_2022_new_data/parameter3_clean_debug_flag_0_2024-08-30.csv"
# # pf = pre-flare
# folder_file_2022_parameter_pf = "Solar_Flare_2022_pre_flare/parameter3_clean_debug_flag_0_2025-04-29.csv"
# folder_file_2023_parameter = "Solar_Flare_2023_new_data/parameter3_clean_debug_flag_0_2024-08-31.csv"
# folder_file_2023_parameter_pf = "Solar_Flare_2023_pre_flare/parameter3_clean_debug_flag_0_2025-04-29.csv"
# 
# # Read in the parameters
# parameters_2011_original_fs <- read.csv(file = paste0(base_path_parameter, folder_file_2011_original_fs_parameter), header = TRUE)
# # Flip the sign
# parameters_2011_original_fs$beta_2 <- parameters_2011_original_fs$beta_2*(-1)
# parameters_2011_original_fs$beta_4 <- parameters_2011_original_fs$beta_4*(-1)
# 
# parameters_2011_old <- read.csv(file = paste0(base_path_parameter, folder_file_2011_old_parameter), header = TRUE)
# parameters_2011_old_r2 <- read.csv(file = paste0(base_path_parameter, folder_file_2011_old_parameter_r2), header = TRUE)
parameters_2011_new <- read.csv(file = paste0(your_path_to_parameters, folder_file_2011_new_parameter), header = TRUE)
# parameters_2022 <- read.csv(file = paste0(base_path_parameter, folder_file_2022_parameter), header = TRUE)
# parameters_2022_pf <- read.csv(file = paste0(base_path_parameter, folder_file_2022_parameter_pf), header = TRUE)
# parameters_2023 <- read.csv(file = paste0(base_path_parameter, folder_file_2023_parameter), header = TRUE)
# parameters_2023_pf <- read.csv(file = paste0(base_path_parameter, folder_file_2023_parameter_pf), header = TRUE)
# 
# ### Data - read in the .rds files
# base_path_data = "~/Documents/GitHub/solar_flare_project/"

# The parameters should be saved in these subfolders
# folder_file_2011_old_data = "data.txt"
# folder_file_2011_new_data = "Processed_Data/2011_Data_Para_Input_Files_New/max_df_2011_new_2024-06-10.rds"
folder_file_2017_data = "Processed_Data/2017_Data_Para_Input_Files_New/max_df_2017_new_2026-02-16.rds"
folder_file_2022_data = "Processed_Data/2022_Data_Para_Input_Files_New/max_df_2022_new_2024-08-29.rds"
folder_file_2023_data = "Processed_Data/2023_Data_Para_Input_Files_New/max_df_2023_new_2024-08-30.rds"
# 
# data_2011_old <- read.table(file = paste0(base_path_data, folder_file_2011_old_data), header = FALSE)
# data_2011_new <- readRDS(file = paste0(base_path_data, folder_file_2011_new_data))
data_2017 <- readRDS(file = paste0(your_path_to_data, folder_file_2017_data))
data_2022 <- readRDS(file = paste0(your_path_to_data, folder_file_2022_data))
data_2023 <- readRDS(file = paste0(your_path_to_data, folder_file_2023_data))

# Work on 11/7/2024
# runif(1, 0, 1000000)
# [1] 227615.3
# Generate 1,000 random Fréchet random variables
set.seed(227615)
r_frechet_draws <- rfrechet(n = 1000, shape = 1)

# Sort the vector
r_frechet_sort <- sort(r_frechet_draws)

# Compare
Empirical <- quantile(r_frechet_sort, probs = c(0.005, 0.025, 0.05, 0.95, 0.975, 0.995))
Theoretical <- qfrechet(p =  c(0.005, 0.025, 0.05, 0.95, 0.975, 0.995), shape = 1)

##### Table S10 #####
as.data.frame(rbind(Empirical, Theoretical)) %>% signif(digits = 4) |> 
  kable(caption = "Comparison of Empirical and Theoretical Fréchet Quantiles", align = "c") %>%
  kable_styling()  %>%
  footnote(number = c("Empirical Quantiles Generated Using 1,000 Random Draws with Random Seed 227615",
                      "Both are Generated from a Unit Frechét Distribution"))

# Load in prediction code
source(paste0(your_path_to_prediction, "prediction_core.R"))

# Run prediction code
# 2017 using 2011 new parameters
data_2017_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2017, input_param_df = parameters_2011_new, alpha = 0.05)

# 2022 using 2011 new parameters
data_2022_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2022, input_param_df = parameters_2011_new, alpha = 0.05)

# 2023 using 2011 new parameters
data_2023_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2023, input_param_df = parameters_2011_new, alpha = 0.05)

# Load in the breach table code
source(paste0(your_path_to_prediction, "create_breach_table_and_viz.R"))

##### Figure S38 ######
breach_2017_forecast_w_parameters_2011_new_95 <- create_breach_table_and_viz_undiff(input_df = data_2017_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 232, forecast_year_and_desc = "2017 Data with 2011 New Parameters")

##### Figure 2 #####
breach_2022_forecast_w_parameters_2011_new_95 <- create_breach_table_and_viz_undiff(input_df = data_2022_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 550, forecast_year_and_desc = "2022 Data with 2011 New Parameters")

##### Figure S39 #####
breach_2023_forecast_w_parameters_2011_new_95 <- create_breach_table_and_viz_undiff(input_df = data_2023_forecast_w_parameters_2011_new_95$predicted_data_df, flare_start_point = 494, forecast_year_and_desc = "2023 Data with 2011 New Parameters")


