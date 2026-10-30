# This code reads in solar flare images downloaded from the internet and summarizes them into a code the model can read.
tic <- Sys.time()

##### USER INPUTS ######

# PLEASE READ! This code contains several "NOTE" items useful for debugging. To run these when running the entire code, make sure the 
# variable below is 1. 
run_debug_code_flag = 1

# Please make sure the filepath below points to the new data
new_filepath = "/Users/stevenmoen/Documents/Cleaned_Flare_Data/2011/"
new_file_name = "flare_2011.csv"

# Please make sure the path below points to your old data, and the old data name is correct
old_filepath = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/"
old_data_name = "data.txt"

# Please specify the output filepath, and make sure the file exists
output_filepath = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Processed_Data/2011_Data_Para_Input_Files_New/"

# Please specify output filename extension
output_name_description = "2011_new"

# The code is organized into steps, which are contained in the master function.

# Read in libraries
library(jpeg)
library(lattice)
library(dplyr)
library(ggplot2)

# Read in the old data
original_data = read.table(paste0(old_filepath, old_data_name),header = F)
# sum(original_data - original_data0)

# Read in the new data
new_data = read.csv(file = paste0(new_filepath, new_file_name), header = F)

# Chop off the first column?

##### STEP 1: Preprocessing #####

#' A function to pre-process the data. It works in three steps.
#' Step i: Cut off the end rows and/or end columns. This is useful if you have one straggler column or row.
#' Step ii: Cut off the bottom of each row. This matrix is long, not wide. It cuts off every nth row to get an even number
#' Step iii: Cut off the top and bottom of each image. This is useful to get the matrix in the required 12 x 10 shape
#'
#' @param input_df - the input dataset, read in directly from Python
#' @param cut_end_rows - number of rows to cut from the end of the dataset. Default is 0, and probably shouldn't be used.
#' @param cut_end_columns - number of columns to cut from the end of the dataset. Default is 0, and may be useful.
#' @param bottom_row_cut_flag - cut every nth row (specified using bottom_row_num)
#' @param bottom_row_num - specify n in cut every nth row
#' @param intended_post_trim_rows - the target number of rows after cutting every nth row, if that option is chosen
#' @param intended_post_trim_columns - target number of columns after previous cuts.
#' @param output_rows - targeted number of output rows
#' @param output_columns - targeted number of output columns
#' @param total_images - total number of images. Used for checking the data.
#' @param post_process_target_rows - the desired end number of rows, after post-processing. Used for checks.
#' @param post_process_target_columns - the desired end number of columns, after post-processing. Used for checks. 
#' @param print_mat_dim_flag - print the final dimensions of the matrix.
#'
#' @return
#' @export
#'
#' @examples
trim_df <- function(input_df, cut_end_rows, cut_end_columns, 
                    bottom_row_cut_flag, bottom_row_num = NULL,
                    intended_post_trim_rows, intended_post_trim_columns,
                    output_rows, output_columns,
                    total_images,
                    post_process_target_rows, post_process_target_columns, 
                    print_mat_dim_flag = 1) {
  # Step i: Trim the end rows and columns.
  trim_df = input_df[1:(nrow(input_df) - cut_end_rows), 1:(ncol(input_df) - cut_end_columns)]
  # Step ii: Trim every nth row.
  if (bottom_row_cut_flag == 1){
    trim_df = trim_df[-seq(from = bottom_row_num, to = nrow(input_df), by = bottom_row_num),]
  }
  # Step iii: Cut off the bottom and top of each image.
  # Trim the images down to size for use in the next code
  # Define the delta
  delta_r = (intended_post_trim_rows - output_rows)/2
  delta_c = (intended_post_trim_columns - output_columns)/2
  # Print errors about the size of the dataframe
  mod_r = delta_r %% 1
  mod_c = delta_c %% 1
  if (mod_r != 0){
    print("WARNING: The difference between the number of post-trim rows and output rows is not divisible by 2. This will likely cause errors. Check your code.")
  }
  if (mod_c != 0){
    print("WARNING: The difference between the number of post-trim columns and output columns is not divisible by 2. This will likely cause errors. Check your code.")
  }
  # Define a storage dataframe
  out_df = as.data.frame(matrix(NA, nrow = output_rows*total_images, ncol = output_columns))
  for (i in 1:total_images){
    # Compute the start and end inclusion values
    start_r = (delta_r + 1) + ((i-1)*intended_post_trim_rows)
    end_r = (intended_post_trim_rows - delta_r) + ((i-1)*intended_post_trim_rows)
    start_c = (delta_c + 1)
    end_c = (intended_post_trim_columns - delta_c)
    # Calculate the storage info
    sto_start_r = ((i-1)*output_rows) + 1
    sto_end_r = (i*output_rows)
    sto_start_c = 1
    sto_end_c = output_columns
    # Input into the storage data frame
    out_df[(sto_start_r:sto_end_r), (sto_start_c:sto_end_c)] = trim_df[(start_r:end_r), (start_c:end_c)]
  }
  # Convert to a matrix
  out_mat = as.matrix(out_df)
  # Check if the matrix is divisible by the target end rows
  mod_target_r = (nrow(out_mat)/total_images) %% post_process_target_rows
  mod_target_c = (ncol(out_mat)) %% post_process_target_columns
  if (mod_target_r != 0){
    print("WARNING: The number of rows per image is not divisible by the post-processing target. This will likely cause errors. Check your code.")
  }
  if (mod_target_c != 0){
    print("WARNING: The number of columns per image is not divisible by the post-processing target.This will likely cause errors. Check your code.")
  }
  # Check for NAs
  na_check = ifelse(any(is.na(out_mat)) == T, 1, 0)
  # Print a warning
  if (na_check == 1){
    print("WARNING: Your output matrix contains NAs, which is indicative of an error. Check your code.")
  }
  # Print the dimensions
  if (print_mat_dim_flag == 1){
    print(dim(out_mat))
  }
  # Return the matrix
  return(out_mat)
}

if (run_debug_code_flag == 1){
  # Run the function
  new_mat <- trim_df(input_df = new_data, cut_end_rows = 0, cut_end_columns = 1, bottom_row_cut_flag = 1, bottom_row_num = 239,
                     intended_post_trim_rows = 238, intended_post_trim_columns = 300,
                     output_rows = 228, output_columns = 300, total_images = 300,
                     post_process_target_rows = 12, post_process_target_columns = 10, print_mat_dim_flag = 1)
}

##### STEP 2: TRANSFORMING THE DATA #####

#' A function that takes the absolute differences between frames for use
#'
#' @param input_mat - the input matrix from the previous function
#' @param input_rows - the number of rows in the raw .jpg file. Default is 239 - don't change this unless you download a different-sized file.
#' @param input_columns  - the number of columns in the raw .jpg file. Default is 512 - don't change this unless you download a different-sized file.
#' @param cut_rows - the number of rows to cut from BOTH the top and the bottom. If 22 is specified, then 22 rows will be cut from both the top and the bottom of the image.
#' @param cut_columns - the number of columns to cut from BOTH the left and the right. If 1 is specified, then 1 column will be cut from both the left and the right of the image.
#'
#' @return - a matrix which contains the elementwise absolute differences between frames
#' @export
#'
#' @examples - abs_diff_mat <- abs_diff_calc(input_mat = all_data_mat)
abs_diff_calc <- function(input_mat, input_rows, input_columns, cut_rows = 0, cut_columns = 0) {
  # Calculate the row and column increments
  row_increments <- (input_rows - 2*cut_rows)
  column_increments <- (input_columns - 2*cut_columns)
  # Initialize a storage matrix - it should have one room for one less image as we are taking the differences
  abs_diff_mat <- matrix(data = NA, nrow = (nrow(input_mat) - row_increments), ncol = column_increments)
  # Compute the n_spots
  n_spots = nrow(abs_diff_mat)/row_increments
  # Populate the matrix
  for (i in 1:n_spots){
    # max(input_mat[((i-1)*row_cut_factor + 1):(i*row_cut_factor), ((j-1)*column_cut_factor + 1):(j*column_cut_factor)])
    abs_diff_mat[((i-1)*row_increments + 1):(i*row_increments),] <- abs(input_mat[(i*row_increments + 1):((i+1)*row_increments),] - input_mat[((i-1)*row_increments + 1):(i*row_increments),])
  }
  # Return the matrix
  return(abs_diff_mat)
}

if (run_debug_code_flag == 1){
  # Run the function
  abs_diff_mat <- abs_diff_calc(input_mat = new_mat, input_rows = 228, input_columns = 300)
}

##### STEP 3: AGGREGATE DATA FUNCTION #####

#' A function that aggregates data after transformation. It takes maxima in moving, non-overlapping windows.
#' The function prints mandatory warnings for unbalanced final matrices (ones that don't have the correct number of rows/columns for reduction),
#' or input files that are too large or two small, resulting in ignored data.
#'
#' @param input_mat - the input matrix after transformations.
#' @param input_rows - the number of rows in the raw .jpg file. Default is 512 - don't change this unless you download a different-sized file.
#' @param input_columns  - the number of columns in the raw .jpg file. Default is 512 - don't change this unless you download a different-sized file.
#' @param cut_rows - the number of rows to cut from BOTH the top and the bottom. If 22 is specified, then 22 rows will be cut from both the top and the bottom of the image.
#' @param cut_columns - the number of columns to cut from BOTH the left and the right. If 1 is specified, then 1 column will be cut from both the left and the right of the image.
#' @param target_end_rows - the targeted number of end rows. Should be 12 to match the previous data
#' @param target_end_columns  - the targeted number of end columns. Should be 10 to match the previous data
#' @param print_messages_flag - a flag that gives you information about the number of images in the output file
#' @param print_debug_flag - a flag that prints information useful for debugging
#'
#' @return - A list containing the max_mat, the widened max_mat (max_mat_wide), and a data frame version of max_mat_wide (max_df)
#' @export
#'
#' @examples - max_mat_out_list <- aggregate_data(input_mat = all_data_mat)
aggregate_data <- function(input_mat, input_rows, input_columns, cut_rows, cut_columns,
                            target_end_rows = 12, target_end_columns = 10, print_messages_flag = 1, print_debug_flag = 0) {
  # Calculate the row and column increments
  row_increments <- (input_rows - 2*cut_rows)
  column_increments <- (input_columns - 2*cut_columns)
  # Calculate the row and column cut factors
  if (row_increments %% target_end_rows == 0){
    row_cut_factor = row_increments/target_end_rows
  } else {
    print("Warning! Modify the cut_rows argument to make sure you have the right number of rows removed from the data to avoid an unbalanced final matrix")
  }
  if (column_increments %% target_end_columns == 0){
    column_cut_factor = column_increments/target_end_columns
  } else {
    print("Warning! Modify the cut_columns argument to make sure you have the right number of columns removed from the data to avoid an unbalanced final matrix")
  }
  # Calculate the number of images
  num_images <- nrow(input_mat)/row_increments
  # Print the number of images for safety
  if(print_messages_flag == 1){
    # Add a plus 1 since we;re working with absolute differences now
    print(paste0("There are ", num_images + 1, " images in the file according to the input parameters"))
  }
  # Print a mandatory warning if num_images isn't a whole number
  if((num_images %% 1 == 0) == 0){
    print("Warning! You have a fractional number of images in this file. Please double-check your inputs.")
  }
  # Write a function to populate the matrix
  max_mat <- matrix(NA, nrow = target_end_rows*num_images, ncol = target_end_columns)
  # Print a warning if there's a mismatch in the input size
  if(nrow(max_mat)*row_cut_factor != nrow(input_mat)){
    print("WARNING: There is a mismatch in the number of intended number of rows in your input data and the actual number of rows in it")
  }
  if(ncol(max_mat)*column_cut_factor != ncol(input_mat)){
    print("WARNING: There is a mismatch in the number of intended number of columns in your input data and the actual number of columns in it")
  }
  # Do a double-loop to populate the matrix
  for (i in 1:nrow(max_mat)){
    for (j in 1:ncol(max_mat)){
      # Populate the matrix
      max_mat[i,j] <- max(input_mat[((i-1)*row_cut_factor + 1):(i*row_cut_factor), ((j-1)*column_cut_factor + 1):(j*column_cut_factor)])
      # Print the ranges
      if (print_debug_flag == 1){
        print(paste0("Start row: ", ((i-1)*row_cut_factor + 1)))
        print(paste0("End row: ", (i*row_cut_factor)))
        print(paste0("Start column: ", ((j-1)*column_cut_factor + 1)))
        print(paste0("End column: ", (j*column_cut_factor)))
      }
    }
  }
  # Convert the max_mat into a format that the next code can read
  max_mat_wide <- max_mat %>% t() %>% matrix(nrow = 1) %>% 
    matrix(nrow = nrow(max_mat)/target_end_rows, ncol = target_end_rows*target_end_columns, byrow = T)
  # Export a data frame as well
  max_df <- max_mat_wide %>% as.data.frame()
  # Return a list containing the max_mat, the widened max_mat (max_mat_wide), and a data frame version of max_mat_wide (max_df)
  return(list(max_mat = max_mat, max_mat_wide = max_mat_wide, max_df = max_df))
}

# NOTE: Run the line below to run the aggregate data function
if (run_debug_code_flag == 1){
  max_mat_out_list <- aggregate_data(input_mat = abs_diff_mat, input_rows = 228, input_columns = 300, cut_rows = 0, cut_columns = 0)
}

# Looks OK. 

##### STEP 4: PLOT HISTOGRAM AND SUMMARY FUNCTION #####

#' This function prints a summary comparing the new vs. the old data, a comparison of quantiles, and a histogram. Both datasets are rescaled
#' by their respective maxima
#'
#' @param old_data_df - the old data
#' @param new_data_mat_or_df - the new data
#' @param print_summary_flag - print a summary
#' @param print_quantile_flag - print a list of quantiles starting at 0% and going to 95% counting by 5%, then 96% through 100% counting by 1%
#' @param print_histogram_flag - prints a histogram comparing the two datasets 
#'
#' @return
#' @export
#'
#' @examples - plot_hist_and_summary(new_data_mat_or_df = all_data_mat, old_data_df = original_data)
plot_hist_and_summary <- function(old_data_df, new_data_mat_or_df,
                                  print_summary_flag = 1, print_quantile_flag = 1, print_histogram_flag = 1) {
  # Flatten the original data into a vector and normalize it by its maxima
  old_data_vec <- old_data_df %>% as.matrix() %>% as.vector()
  old_data_vec <- old_data_vec/max(old_data_vec)
  # Flatten the new data into a vector and normalize it by its maxima
  ## If there's a df flag active, include an as.matrix command
  new_data_vec <- new_data_mat_or_df %>% as.matrix() %>% as.vector()
  new_data_vec <- new_data_vec/max(new_data_vec)
  # Print the summaries
  if (print_summary_flag == 1){
    print("Old Data Summary")
    summary(old_data_vec) %>% print()
    print("New Data Summary")
    summary(new_data_vec) %>% print()
  }
  # Print the quantiles
  if (print_summary_flag == 1){
    print("Old Data Quantiles")
    quantile(old_data_vec, probs = c(seq(0, 0.95, 0.05), seq(0.96, 1, 0.01))) %>% signif(3) %>% print()
    print("New Data Quantiles")
    quantile(new_data_vec, probs = c(seq(0, 0.95, 0.05), seq(0.96, 1, 0.01))) %>% signif(3) %>% print()
  }
  if (print_histogram_flag == 1 ){
    # Create a data frame
    visualization_df <- data.frame(normalized_brightness = c(new_data_vec, old_data_vec), label = c(rep("New_Data", length(new_data_vec)), rep("Old_Data", length(old_data_vec))))
    # Plot interleaved histograms
    ggplot(visualization_df, aes(x=normalized_brightness, color=label)) +
      geom_density() + 
      labs(x = "Normalized Brightness", color = "Label")
  }
}

# NOTE: Run this line to visualize the summarized data
if (run_debug_code_flag == 1){
  plot_hist_and_summary(new_data_mat_or_df = max_mat_out_list$max_df, old_data_df = original_data)
}

# Export the data frame as an .rds file
if (run_debug_code_flag == 1){
  saveRDS(max_mat_out_list$max_df, paste0(output_filepath, "max_df_", export_file_description = output_name_description, "_", Sys.Date(), ".rds"))
}
