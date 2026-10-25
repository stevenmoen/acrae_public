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
folder_file_2017_parameter_pp = "Solar_Flare_2017_cut_burnin10/parameter3_clean_debug_flag_0_2026-08-22.csv" # Old, long-burn in
folder_file_2022_parameter_pp = "Solar_Flare_2022_cut/parameter3_clean_debug_flag_0_2026-08-21.csv"
folder_file_2023_parameter_pp = "Solar_Flare_2023_cut/parameter3_clean_debug_flag_0_2026-08-21.csv"
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
parameters_2017_pp <- read.csv(file = paste0(your_path_to_parameters, folder_file_2017_parameter_pp), header = TRUE)
parameters_2022_pp <- read.csv(file = paste0(your_path_to_parameters, folder_file_2022_parameter_pp), header = TRUE)
parameters_2023_pp <- read.csv(file = paste0(your_path_to_parameters, folder_file_2023_parameter_pp), header = TRUE)
# 
# ### Data - read in the .rds files
# base_path_data = "~/Documents/GitHub/solar_flare_project/"

# The parameters should be saved in these subfolders
# folder_file_2011_old_data = "data.txt"
folder_file_2011_new_data = "Processed_Data/2011_Data_Para_Input_Files_New/max_df_2011_new_2024-06-10.rds"
folder_file_2017_data = "Processed_Data/2017_Data_Para_Input_Files_New/max_df_2017_new_2026-02-16.rds"
folder_file_2022_data = "Processed_Data/2022_Data_Para_Input_Files_New/max_df_2022_new_2024-08-29.rds"
folder_file_2023_data = "Processed_Data/2023_Data_Para_Input_Files_New/max_df_2023_new_2024-08-30.rds"
folder_file_2017_data_test = "Processed_Data/post_peak_testing_data/max_df_2017_testing.rds"
folder_file_2022_data_test = "Processed_Data/post_peak_testing_data/max_df_2022_testing.rds"
folder_file_2023_data_test = "Processed_Data/post_peak_testing_data/max_df_2023_testing.rds"

# data_2011_old <- read.table(file = paste0(base_path_data, folder_file_2011_old_data), header = FALSE)
# Whole period data
data_2011_new <- readRDS(file = paste0(your_path_to_data, folder_file_2011_new_data))
data_2017 <- readRDS(file = paste0(your_path_to_data, folder_file_2017_data))
data_2022 <- readRDS(file = paste0(your_path_to_data, folder_file_2022_data))
data_2023 <- readRDS(file = paste0(your_path_to_data, folder_file_2023_data))
# Test period data
data_2017_test <- readRDS(file = paste0(your_path_to_data, folder_file_2017_data_test))
data_2022_test <- readRDS(file = paste0(your_path_to_data, folder_file_2022_data_test))
data_2023_test <- readRDS(file = paste0(your_path_to_data, folder_file_2023_data_test))

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

# 2011 whole period data
## 2011 new parameters
data_2011_new_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2011_new, input_param_df = parameters_2011_new, alpha = 0.1)
data_2011_new_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2011_new, input_param_df = parameters_2011_new, alpha = 0.05)
data_2011_new_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2011_new, input_param_df = parameters_2011_new, alpha = 0.01)

# 2017 whole period data
## 2011 new parameters
data_2017_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2017, input_param_df = parameters_2011_new, alpha = 0.1)
data_2017_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2017, input_param_df = parameters_2011_new, alpha = 0.05)
data_2017_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2017, input_param_df = parameters_2011_new, alpha = 0.01)

# 2022 whole period data
## 2011 new parameters
data_2022_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2022, input_param_df = parameters_2011_new, alpha = 0.1)
data_2022_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2022, input_param_df = parameters_2011_new, alpha = 0.05)
data_2022_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2022, input_param_df = parameters_2011_new, alpha = 0.01)

# 2023 whole period data
## 2011 new parameters
data_2023_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2023, input_param_df = parameters_2011_new, alpha = 0.1)
data_2023_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2023, input_param_df = parameters_2011_new, alpha = 0.05)
data_2023_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2023, input_param_df = parameters_2011_new, alpha = 0.01)

# 2017 pre-peak data 
## 2017 pre-peak parameters
data_2017_pp_forecast_w_parameters_2017_pp_90 <- prediction_with_max(input_data_df = data_2017_test, input_param_df = parameters_2017_pp, alpha = 0.10)
data_2017_pp_forecast_w_parameters_2017_pp_95 <- prediction_with_max(input_data_df = data_2017_test, input_param_df = parameters_2017_pp, alpha = 0.05)
data_2017_pp_forecast_w_parameters_2017_pp_99 <- prediction_with_max(input_data_df = data_2017_test, input_param_df = parameters_2017_pp, alpha = 0.01)

## 2011 new parameters
data_2017_pp_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2017_test, input_param_df = parameters_2011_new, alpha = 0.10)
data_2017_pp_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2017_test, input_param_df = parameters_2011_new, alpha = 0.05)
data_2017_pp_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2017_test, input_param_df = parameters_2011_new, alpha = 0.01)

# 2022 pre-peak data 
## 2022 pre-peak parameters
data_2022_pp_forecast_w_parameters_2022_pp_90 <- prediction_with_max(input_data_df = data_2022_test, input_param_df = parameters_2022_pp, alpha = 0.10)
data_2022_pp_forecast_w_parameters_2022_pp_95 <- prediction_with_max(input_data_df = data_2022_test, input_param_df = parameters_2022_pp, alpha = 0.05)
data_2022_pp_forecast_w_parameters_2022_pp_99 <- prediction_with_max(input_data_df = data_2022_test, input_param_df = parameters_2022_pp, alpha = 0.01)

## 2011 new parameters
data_2022_pp_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2022_test, input_param_df = parameters_2011_new, alpha = 0.10)
data_2022_pp_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2022_test, input_param_df = parameters_2011_new, alpha = 0.05)
data_2022_pp_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2022_test, input_param_df = parameters_2011_new, alpha = 0.01)

# 2022 pre-peak data 
## 2023 pre-peak parameters
data_2023_pp_forecast_w_parameters_2023_pp_90 <- prediction_with_max(input_data_df = data_2023_test, input_param_df = parameters_2023_pp, alpha = 0.10)
data_2023_pp_forecast_w_parameters_2023_pp_95 <- prediction_with_max(input_data_df = data_2023_test, input_param_df = parameters_2023_pp, alpha = 0.05)
data_2023_pp_forecast_w_parameters_2023_pp_99 <- prediction_with_max(input_data_df = data_2023_test, input_param_df = parameters_2023_pp, alpha = 0.01)

## 2011 new parameters
data_2023_pp_forecast_w_parameters_2011_new_90 <- prediction_with_max(input_data_df = data_2023_test, input_param_df = parameters_2011_new, alpha = 0.10)
data_2023_pp_forecast_w_parameters_2011_new_95 <- prediction_with_max(input_data_df = data_2023_test, input_param_df = parameters_2011_new, alpha = 0.05)
data_2023_pp_forecast_w_parameters_2011_new_99 <- prediction_with_max(input_data_df = data_2023_test, input_param_df = parameters_2011_new, alpha = 0.01)

##### Table 2 #####
## 2011 Whole (first row)
data_2011_new_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2011_new_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2011_new_forecast_w_parameters_2011_new_99$coverage_table_grand

## 2017 Whole (second row)
data_2017_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2017_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2017_forecast_w_parameters_2011_new_99$coverage_table_grand

## 2017 Post-peak (third row)
data_2017_pp_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2017_pp_forecast_w_parameters_2017_pp_90$coverage_table_grand
data_2017_pp_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2017_pp_forecast_w_parameters_2017_pp_95$coverage_table_grand
data_2017_pp_forecast_w_parameters_2011_new_99$coverage_table_grand
data_2017_pp_forecast_w_parameters_2017_pp_99$coverage_table_grand

## 2022 Whole (fourth row)
data_2022_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2022_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2022_forecast_w_parameters_2011_new_99$coverage_table_grand

## 2022 Post-peak (fifth row)
data_2022_pp_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2022_pp_forecast_w_parameters_2022_pp_90$coverage_table_grand
data_2022_pp_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2022_pp_forecast_w_parameters_2022_pp_95$coverage_table_grand
data_2022_pp_forecast_w_parameters_2011_new_99$coverage_table_grand
data_2022_pp_forecast_w_parameters_2022_pp_99$coverage_table_grand

## 2023 Whole (sixth row)
data_2023_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2023_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2023_forecast_w_parameters_2011_new_99$coverage_table_grand

## 2023 Post-peak (seventh row)
data_2023_pp_forecast_w_parameters_2011_new_90$coverage_table_grand
data_2023_pp_forecast_w_parameters_2023_pp_90$coverage_table_grand
data_2023_pp_forecast_w_parameters_2011_new_95$coverage_table_grand
data_2023_pp_forecast_w_parameters_2023_pp_95$coverage_table_grand
data_2023_pp_forecast_w_parameters_2011_new_99$coverage_table_grand
data_2023_pp_forecast_w_parameters_2023_pp_99$coverage_table_grand

# Load in table formatting code
source(paste0(your_path_to_prediction, "create_coverage_table.R"))

##### Figure S25 ######
create_coverage_table(data_2011_new_forecast_w_parameters_2011_new_90, year_data = 2011, year_parameters = 2011, alpha = 0.1)

##### Figure S26 ######
create_coverage_table(data_2011_new_forecast_w_parameters_2011_new_95, year_data = 2011, year_parameters = 2011, alpha = 0.05)

##### Figure S27 ######
create_coverage_table(data_2011_new_forecast_w_parameters_2011_new_99, year_data = 2011, year_parameters = 2011, alpha = 0.01)

##### Figure S28 ######
create_coverage_table(data_2017_forecast_w_parameters_2011_new_90, year_data = 2017, year_parameters = 2011, alpha = 0.1)

##### Figure S29 ######
create_coverage_table(data_2017_forecast_w_parameters_2011_new_95, year_data = 2017, year_parameters = 2011, alpha = 0.05)

##### Figure S30 ######
create_coverage_table(data_2017_forecast_w_parameters_2011_new_99, year_data = 2017, year_parameters = 2011, alpha = 0.01)

##### Figure S31 ######
create_coverage_table(data_2022_forecast_w_parameters_2011_new_90, year_data = 2022, year_parameters = 2011, alpha = 0.1)

##### Figure S32 ######
create_coverage_table(data_2022_forecast_w_parameters_2011_new_95, year_data = 2022, year_parameters = 2011, alpha = 0.05)

##### Figure S33 ######
create_coverage_table(data_2022_forecast_w_parameters_2011_new_99, year_data = 2022, year_parameters = 2011, alpha = 0.01)

##### Figure S34 ######
create_coverage_table(data_2023_forecast_w_parameters_2011_new_90, year_data = 2023, year_parameters = 2011, alpha = 0.1)

##### Figure S35 ######
create_coverage_table(data_2023_forecast_w_parameters_2011_new_95, year_data = 2023, year_parameters = 2011, alpha = 0.05)

##### Figure S36 ######
create_coverage_table(data_2023_forecast_w_parameters_2011_new_99, year_data = 2023, year_parameters = 2011, alpha = 0.01)


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

