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