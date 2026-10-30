# File to cut data for parameter estimation - 2017

# Please update the location to where the clean data is stored
input_filepath = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Processed_Data/2022_Data_Para_Input_Files_New/"
input_filename = "max_df_2022_new_2024-08-29.rds"

# Define an output file
output_filepath = "~/Documents/GitHub/solar_flare_project/Data_Cutting_for_Prediction_MR_R2/"
output_filename = "max_df_2022_cut.rds"

# Define the point to cut
cutting_point = 550
# To arrive at this number, we take the peak brightness on the undifferenced scale, and subtract 2.
# 1 for moving to the differenced scale, 1 for allowing predictability.

# This is where you specify the data file - read in the rds file
data_new_raw = readRDS(paste0(input_filepath, input_filename))

# Cut the data
data_prepeak_cut = data_new_raw[1:cutting_point,]

# Save the data
# saveRDS(data_prepeak_cut, paste0(output_filepath, output_filename))

# Include data after the cutting point
output_filepath_test = "~/Documents/GitHub/solar_flare_project/Data_Cutting_for_Prediction_MR_R2/testing_data/"
output_filename_test = "max_df_2022_testing.rds"

# Cut the data
data_postpeak_cut = data_new_raw[(cutting_point+1):nrow(data_new_raw),]

# Save the data
# saveRDS(data_postpeak_cut, paste0(output_filepath_test, output_filename_test))

