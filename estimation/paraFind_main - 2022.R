# Record the start time
time_start=Sys.time()
## This code took 4-5 hours on Jili's computer

##### USER INPUTS ######

# Please update the location to where the clean data is stored
input_filepath = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Processed_Data/2022_Data_Para_Input_Files_New/"
input_filename = "max_df_2022_new_2024-08-29.rds"

# Please specify a filepath to write the files to. 
# NOTE: Make sure this exists!
filepath_out = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Estimated_Parameters/Solar_Flare_2022_new_data/"

# Please set a working directory (don't change this)
setwd("/Users/stevenmoen/Documents/GitHub/solar_flare_project/")

# Please indicate whether you want to receive a push notification using pushoverr when the code is finished
push_notification_flag = 1

# Please specify input parameters for the push notification (don't change this casually)
library(pushoverr)
set_pushover_user(user = "u63qya9hy3p18zn5qip7sfyiwkpj35")
set_pushover_app(token = "a2afqw1zs1prmnj5hu7ifuzijn5upn")

# Please the seed
set.seed(08292024) # Use the date of the run.

# Please indicate if you want to run the code for all 120 sites. (0 = run all sites, 1 = run first site.)
run_debug_flag = 0

# Please indicate whether you want to print information on how long things take to the screen
print_debug_flag = 1

# Please indicate the number of rows and columns in your output dataset (don't change this)
nrow_out = 12
ncol_out = 10


# Please indicate a eps_tan to avoid running into issues with infinities
eps_tan <- 1e-8

# Please specify if you want to use the exponential of the raw data in the code
# NOTE THAT THIS OVERWRITES THE rescale_to_old_flag AND use_exponential_data_flag
use_tangent_tf_data_flag = 0

# Please specify if you want to use the exponential of the raw data in the code
# NOTE THAT THIS OVERWRITES THE rescale_to_old_flag. If use_tangent_tf_data_flag is on, then this doesn't matter.
use_exponential_data_flag = 0

# Please indicate if you want to transform the data by multiplying the data by the max of data_old
# NOTE that if use_exponential_data_flag = 1, then this flag doesn't matter.
rescale_to_old_flag = 0

# Please indicate whether you want a visualization of the transformed data
print_visualization_flag = 1

# Please indicate whether you want a log exported
export_log_flag = 1

##### SETUP #####

# Initialize a storage list
log_storage_list <- list()
# append(list,values)

# Initialize a counter for the print statement
count_var <- 1

# Load libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

# This is where you specify the data file - read in the rds file
data_new_raw = readRDS(paste0(input_filepath, input_filename))
data_old = read.table("data.txt",header = F) # Old
max(data_old)
min(data_old)


# Rescale the data by the max and add 1
if (rescale_to_old_flag == 1){
  data <- data_new_raw*(max(data_old)/max(data_new_raw)) + min(data_old)
} else {
  data <- data_new_raw
}

## Problem solving on the tangent transformation
# Look at the frechet vector
# rf_vec_norm <- rfrechet(10000, location = 0, scale = 1, shape = 1)
# rf_vec_norm <- rf_vec_norm/max(rf_vec_norm)
# hist(rf_vec_norm, breaks = seq(0,1, 0.01))
# 
# data_new_vec <- as.vector(as.matrix(data_new_raw))
# data_new_vec_tan <- tan((data_new_vec + 1e-6)*(pi/(2 + 1e-6)))
# data_new_vec_tan_norm <- data_new_vec_tan/max(data_new_vec_tan)
# hist(data_new_vec_tan_norm, breaks = seq(0,1, 0.01))

# These are roughly comparable? Might be worth trying.

# Create a side dataset where we take the exponential of the raw data
data_exp <- exp(data_new_raw)

# 
#' Transforms the data using a tangent
#'
#' @param data - raw data
#' @param eps - a factor to avoid infinities in the function. Default is 1e-8
#'
#' @return
#' @export
#'
#' @examples
tan_transform <- function(data, eps = eps_tan) {
  # Transform the data using a tangent
  # This ensures the data non-negative.
  data_tan <- tan(((data + eps)/(1 + eps))*(pi/(2 + eps)))
  # data_tan <- tan(data*(pi/(2 + eps)))
  # Export the dataset
  return(data_tan)
}


data_tan <- tan_transform(data = data_new_raw)
# max(data_tan)
summary(data_tan)

#' This function prints a summary comparing the new vs. the old data, a comparison of quantiles, and a histogram. 
#' There is an optional rescaling argument to adjust them by their respective maxima
#'
#' @param old_data_df - the old data
#' @param new_data_mat_or_df - the new data
#' @param print_summary_flag - print a summary
#' @param print_quantile_flag - print a list of quantiles starting at 0% and going to 95% counting by 5%, then 96% through 100% counting by 1%
#' @param print_histogram_flag - prints a histogram comparing the two datasets 
#' @param norm_flag - a flag that normalizes the data. Turn this off if you want to see if the data are the same scale
#' @param print_new_ts_visualization_flag - print a visualization of the new individual time series, grouped by 10
#' @param print_old_ts_visualization_flag - print a visualization of the old individual time series, grouped by 10
#' @param print_new_histogram_flag - print a visualization of the histograms of the new data, grouped by 10
#' @param print_old_histogram_flag - print a visualization of the histograms of the old data, grouped by 10
#'
#' @return
#' @export
#'
#' @examples - plot_hist_and_summary_opt_norm(old_data_df = data_old, new_data_mat_or_df = data)
plot_hist_and_summary_opt_norm <- function(old_data_df, new_data_mat_or_df,
                                  print_summary_flag = 1, print_quantile_flag = 1, print_histogram_flag = 1, norm_flag = 0,
                                  print_new_ts_visualization_flag = 1, print_old_ts_visualization_flag = 1, 
                                  print_new_histogram_flag = 1, print_old_histogram_flag = 1,
                                  print_one_new_ts_visualization_flag = 1, print_one_new_histogram_flag = 1,
                                  new_row = 11, new_column = 2) {
  # Flatten the original data into a vector and normalize it by its maxima (if the normalization flag is turned on)
  old_data_vec <- old_data_df %>% as.matrix() %>% as.vector()
  if (norm_flag == 1){
    old_data_vec <- old_data_vec/max(old_data_vec)
  }
  # Flatten the new data into a vector and normalize it by its maxima (if the normalization flag is turned on)
  ## If there's a df flag active, include an as.matrix command
  new_data_vec <- new_data_mat_or_df %>% as.matrix() %>% as.vector()
  if (norm_flag == 1){
    new_data_vec <- new_data_vec/max(new_data_vec)
  }
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
  # Visualize non-normalized interleaved histograms
  if (print_histogram_flag == 1 & norm_flag == 0){
    # Create a data frame
    visualization_df_hist <- data.frame(normalized_brightness = c(new_data_vec, old_data_vec), label = c(rep("New_Data", length(new_data_vec)), rep("Old_Data", length(old_data_vec))))
    # Plot interleaved histograms
    print(ggplot(visualization_df_hist, aes(x=normalized_brightness, color=label)) +
      geom_density() + 
      labs(x = "Brightness", color = "Label"))
  }
  # Visualize normalized interleaved histograms
  if (print_histogram_flag == 1 & norm_flag == 1){
    # Create a data frame
    visualization_df_hist <- data.frame(normalized_brightness = c(new_data_vec, old_data_vec), label = c(rep("New_Data", length(new_data_vec)), rep("Old_Data", length(old_data_vec))))
    # Plot interleaved histograms
    print(ggplot(visualization_df_hist, aes(x=normalized_brightness, color=label)) +
      geom_density() + 
      labs(x = "Normalized Brightness", color = "Label"))
  }
  # Visualize the time series of the new data
  if (print_new_ts_visualization_flag == 1){
    # Make the data longer
    long_data <- {{new_data_mat_or_df}} %>% pivot_longer(cols = starts_with("V"),
                                       names_to = "Location",
                                       names_prefix = "V",
                                       values_to = "Brightness") %>% 
      # Create a numeric location, and a group column called Row. This is the row number.
      mutate(Location_Num = as.numeric(Location),
             Row = ceiling(Location_Num/10))
    # Append labels for visualization
    # Image number is the number of images, which should align with raw data minus 1.
    # Column is the column. max(Location_Num) is 120, and max(Row) is 10
    # This is repeated for num_images*12, and then converted to a factor
    visualization_df_new_ts <- long_data %>% mutate(Image_Number = rep(1:(nrow(long_data)/max(Location_Num)), each = max(Location_Num)),
                         Column_num = rep(seq(1:(max(Location_Num)/max(Row))),(max(Row))*(nrow(long_data)/max(Location_Num))),
                         Column = as.factor(Column_num)) %>% 
      # Sort the data
      arrange(Location_Num)
    # Create a visualization
    print(ggplot(data = visualization_df_new_ts, aes(x = Image_Number, y = Brightness, color = Column, group = Column)) + 
      geom_line(linetype = "dashed") + facet_wrap(vars(Row), labeller = label_both) +
      labs(x = "Image Number", y = "Response Variable", color = "Column", title = "Time Series of New Data"))
    # Repeat the process for the old data
  }
  # Visualize the time series of the old data
  if (print_old_ts_visualization_flag == 1){
    # Make the data longer
    long_data <- {{old_data_df}} %>% pivot_longer(cols = starts_with("V"),
                                                         names_to = "Location",
                                                         names_prefix = "V",
                                                         values_to = "Brightness") %>% 
      # Create a numeric location, and a group column called Row. This is the row number.
      mutate(Location_Num = as.numeric(Location),
             Row = ceiling(Location_Num/10))
    # Append labels for visualization
    # Image number is the number of images, which should align with raw data minus 1.
    # Column is the column. max(Location_Num) is 120, and max(Row) is 10
    # This is repeated for num_images*12, and then converted to a factor
    visualization_df_old_ts <- long_data %>% mutate(Image_Number = rep(1:(nrow(long_data)/max(Location_Num)), each = max(Location_Num)),
                         Column_num = rep(seq(1:(max(Location_Num)/max(Row))),(max(Row))*(nrow(long_data)/max(Location_Num))),
                         Column = as.factor(Column_num)) %>% 
      # Sort the data
      arrange(Location_Num)
      # Create a visualization
    print(ggplot(data = visualization_df_old_ts, aes(x = Image_Number, y = Brightness, color = Column, group = Column)) + 
      geom_line(linetype = "dashed") + facet_wrap(vars(Row), labeller = label_both) +
      labs(x = "Image Number", y = "Response Variable", color = "Column", title = "Time Series of Old Data"))
  }
  # Visualize the histogram of the new data
  if (print_new_histogram_flag == 1){
    print(ggplot(data = visualization_df_new_ts, aes(x = Brightness, color = Column, group = Column)) + 
            geom_density() + facet_wrap(vars(Row), labeller = label_both) +
            labs(x = "Response Variable", color = "Column", title = "Histograms of New Data"))
  }
  # Visualize the histogram of the old data
  if (print_old_histogram_flag == 1){
    print(ggplot(data = visualization_df_old_ts, aes(x = Brightness, color = Column, group = Column)) + 
            geom_density() + facet_wrap(vars(Row), labeller = label_both) +
            labs(x = "Response Variable", color = "Column", title = "Histograms of Old Data"))
  }
  # Create a new dataframe
  # Note that Row is a numeric variable, whereas Column is a factor. That's why it calls Column_num
  visualization_df_one_new_ts <- visualization_df_new_ts %>% filter(Row == new_row, Column_num == new_column)
  # Visualize one time series plot of the new data
  if (print_one_new_ts_visualization_flag == 1){
    # Create a visualization
    print(ggplot(data = visualization_df_one_new_ts, aes(x = Image_Number, y = Brightness, color = Column, group = Column)) + 
            geom_line() + facet_wrap(vars(Row), labeller = label_both) +
            labs(x = "Image Number", y = "Response Variable", color = "Column", title = paste0("Time Series of New Data, Row ", new_row, ", Column ", new_column)))
    # Repeat the process for the old data
  }
  # Visualize one histogram of the new data
  if (print_one_new_histogram_flag == 1){
    # Create a visualization
    print(ggplot(data = visualization_df_one_new_ts, aes(x = Brightness, color = Column, group = Column)) + 
            geom_density() + facet_wrap(vars(Row), labeller = label_both) +
            labs(x = "Response Variable", color = "Column", title = paste0("Histogram of New Data, Row ", new_row, ", Column ", new_column)))
  }
    # Repeat the process for the old data
  
  
}


# Visualize the data
if(print_visualization_flag == 1){
  plot_hist_and_summary_opt_norm(old_data_df = data_old, new_data_mat_or_df = data)
}


# Try another with the exponential data
# if(print_visualization_flag == 1){
#   plot_hist_and_summary_opt_norm(old_data_df = data_old, new_data_mat_or_df = data_tan, norm_flag = 1)
# }

# I don't know if this will make a difference. But we've got to check.

# Assign data_exp to be data
if(use_exponential_data_flag == 1){
  data <- data_exp
}

if(use_tangent_tf_data_flag == 1){
  data <- data_tan
}


# Load in the other file
source("SimMLE.R")

# Load relevant libraries
library(GenSA)
library(foreach)
library(doParallel)

# Sets up parallelization (?)
registerDoParallel(2)

#  Extends the number of lines to print
options(scipen=999)

# Reads in the data and sets it as a matrix
dat = as.matrix(data)

# Re-assign the matrix
Rall = dat # 299 rows by 120 columns
Rall2 = matrix(0,nrow=nrow(data),ncol=120) # Old
# Rall2 = matrix(0,nrow=299,ncol=120) # Old

# Record a time for the start of the maxima section
time_start_maxima <- Sys.time()

##### COMPUTING MAXIMA OF SURROUNDING DATA #####

for (i in 1:120){
  # This is the top row excluding the left or right-hand side corners
  if (i %in% c(2,3,4,5,6,7,8,9)){
    # For 2, this selects columns 1, 3, 11, 12, and 13, which is represented below
    # 0 = included, X = selected square, - = excluded
    # 0 X 0 - ...
    # 0 0 0 - ...
    # - - - - ...
    Rtemp = Rall[,c(i-1,i+1,i+10,i+9,i+11)]
    # Computes the maxima over these squares
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # This is the bottom row excluding the left or right-hand side corners
  else if (i %in% c(112,113,114,115,116,117,118,119)){
    # For 112, this selects 111, 113, 102, 101, 103, which is represented below
    # 0 = included, X = selected square, - = excluded
    # - - - - ...
    # 0 0 0 - ...
    # 0 X 0 - ...
    Rtemp = Rall[,c(i-1,i+1,i-10,i-9,i-11)]
    # Computes the maxima over these squares
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # Left-hand side column, excluding the corners
  else if(i %in% c(11,21,31,41,51,61,71,81,91,101)){
    # For 11, this selects 12, 1, 21, 2, and 22
    Rtemp = Rall[,c(i+1,i-10,i+10,i-9,i+11)]
    # Computes the maxima over these squares
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # Right-hand side column, excluding the corners
  else if (i %in% c(20,30,40,50,60,70,80,90,100,110)){
    # For 20, this selects 19, 30, 10, 29, and 9
    Rtemp = Rall[,c(i-1,i+10,i-10,i+9,i-11)]
    # Computes the maxima over these squares
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # Upper left-hand corner
  else if (i == 1){
    # Selects the cells around it as before
    Rtemp = Rall[,c(i+1,i+10,i+11)]
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # Upper right-hand corner
  else if (i == 10){
    # Selects the cells around it as before
    Rtemp = Rall[,c(i-1,i+10,i+9)]
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # Lower left-hand corner
  else if (i == 111){
    # Selects the cells around it as before
    Rtemp = Rall[,c(i+1,i-9,i-10)]
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # Lower right-hand corner
  else if (i == 120){
    # Selects the cells around it as before
    Rtemp = Rall[,c(i-1,i-10,i-11)]
    Rall2[,i] = apply(Rtemp,1,max)
  }
  # This is everything else; should be everything not on the boundary
  else{
    Rtemp = Rall[,c(i-1,i+1,i-10,i+10,i-11,i+11,i-9,i+9)]
    Rall2[,i] = apply(Rtemp,1,max)
  }

}

# This computes a "max" matrix for the neighbors and stores it in Rall2.

# Record a time for the start of the maxima section
time_start_first_opt <- Sys.time()

# Append the storage list
log_storage_list[[count_var]] <- paste0("Start of the max computation: ", time_start_maxima)

# Print the first message, if indicated
if(print_debug_flag == 1){
  log_storage_list[[count_var]] %>% print()
}

# Update the count variable
count_var <- count_var + 1

# Append the storage list
log_storage_list[[count_var]] <- paste0("Start of the first optimization : ", time_start_first_opt)

# Print the entry, if indicated
if(print_debug_flag == 1){
  log_storage_list[[count_var]] %>% print()
}

# Update the count variable
count_var <- count_var + 1

##### FIRST OPTIMIZATION ROUTINE #####

m = 0

### ADJUSTMENT 2: Initializing a storage matrix called rMatrix
rMatrix = matrix(0, nrow = 120, ncol = 14)


# Put in flag to only run the code once
if (run_debug_flag == 1){
  max_iter_fo = 1
  } else {
  max_iter_fo = 120
}

# Parameter info - these lines can be found directly from SimMLE.R
# gamma0=para[1];gamma1=para[2];gamma2=para[3];gamma3=para[4];gamma4=para[5];gamma5=para[6]
# beta0=para[7];beta1=para[8];beta2=para[9];beta3=para[10];beta4=para[11];beta5=para[12]

# theta[13] is mu

for (j in 1:max_iter_fo){

  # Append the storage list
  log_storage_list[[count_var]] <- paste0("Column ", j, " in the first optimization routine started running at ", Sys.time())
  
  # Print the entry, if indicated
  if(print_debug_flag == 1){
    log_storage_list[[count_var]] %>% print()
  }
  
  # Update the count variable
  count_var <- count_var + 1
  
  # if(print_debug_flag == 1){
  #   print(paste0("Column ", j, " in the first optimization routine started running at ", Sys.time()))
  # }
      
      # print(j)
      m = m+1
      
      # Pulls the jth column of the raw data
      R = Rall[,j]
      # Pulls the jth column of the "max" data
      R2 = Rall2[,j]
      
      # These establish minimum and maximum values for the function to converge properly.
      # g = gamma, b = beta
      #    g_0,   g_1,   g_2,           g_3,   g_4,            g_5, b_0,   b_1,    b_2,           b_3,    b_4,            b_5,  theta
      lb=c(-10, 0.001, 0.001, exp(1)/min(R), 0.001, exp(1)/min(R2), -10, 0.001,    -40, exp(1)/min(R),    -40, exp(1)/min(R2),  0.001)
      ub=c( 10, 0.999,    40,            40,    40,             40,  10, 0.999, -0.001,            40, -0.001,             40, min(R))
      
      # This references the Acf.R file; we'll look at that next
      source('AcF.R')
      result
      
      # Append the storage list
      log_storage_list[[count_var]] <- paste0("Column ", j, " got past the first result in the first optimization routine at ", Sys.time())
      
      # Print the entry, if indicated
      if(print_debug_flag == 1){
        log_storage_list[[count_var]] %>% print()
      }
      
      # Update the count variable
      count_var <- count_var + 1
      
      # if(print_debug_flag == 1){
      #   print(paste0("Column ", j, " got past the first result in the first optimization routine at ", Sys.time()))
      # }
      
      # Sets the values of b_1 to 0 if it's on the boundary of the parameter space. 
      if (result[8] == 0.001 || result[8] == 0.999){
        # g = gamma, b = beta
        #    g_0,   g_1,   g_2,           g_3,   g_4,            g_5, b_0, b_1,    b_2,           b_3,    b_4,            b_5,  theta
        lb=c(-10, 0.001, 0.001, exp(1)/min(R), 0.001, exp(1)/min(R2), -10,   0,    -40, exp(1)/min(R),    -40, exp(1)/min(R2),  0.001)
        ub=c( 10, 0.999,    40,            40,    40,             40,  20,   0, -0.001,            40, -0.001,             40, min(R))
        source('AcF.R')
       
      } 
      
      
      # Append the storage list
      log_storage_list[[count_var]] <- paste0("Column ", j, " got past the second result in the first optimization routine at ", Sys.time())
      
      # Print the entry, if indicated
      if(print_debug_flag == 1){
        log_storage_list[[count_var]] %>% print()
      }
      
      # Update the count variable
      count_var <- count_var + 1
      
      # 
      # if(print_debug_flag == 1){
      #   print(paste0("Column ", j, " got past the second result in the first optimization routine at ", Sys.time()))
      # }
      
      # Sets the values of g_1 to 0 if it's on the boundary of the parameter space. 
      if (result[2] == 0.001 || result[2] == 0.999){
        # g = gamma, b = beta
        #    g_0, g_1,   g_2,           g_3,   g_4,            g_5, b_0, b_1,    b_2,           b_3,    b_4,            b_5,  theta
        lb=c(-10,   0, 0.001, exp(1)/min(R), 0.001, exp(1)/min(R2), -10,   0,    -40, exp(1)/min(R),    -40, exp(1)/min(R2),  0.001)
        ub=c( 10,   0,    40,            40,    40,             40,  10,   0, -0.001,            40, -0.001,             40, min(R))
        source('AcF.R')
      }
      
      
      # Append the storage list
      log_storage_list[[count_var]] <- paste0("Column ", j, " got past the third result in the first optimization routine at ", Sys.time())
      
      # Print the entry, if indicated
      if(print_debug_flag == 1){
        log_storage_list[[count_var]] %>% print()
      }
      
      # Update the count variable
      count_var <- count_var + 1
      
      # 
      # if(print_debug_flag == 1){
      #   print(paste0("Column ", j, " got past the third result in the first optimization routine at ", Sys.time()))
      # }
      
      # The rMatrix is used to store all of the parameters
      rMatrix[m,] = result
      


}


# Record a time for the start of the maxima section
time_start_second_opt <- Sys.time()


# Append the storage list
log_storage_list[[count_var]] <- paste0("Start of the second optimization : ", time_start_second_opt)

# Print the entry, if indicated
if(print_debug_flag == 1){
  log_storage_list[[count_var]] %>% print()
}

# Update the count variable
count_var <- count_var + 1

# if(print_debug_flag == 1){
#   print(paste0("Start of the first optimization start: ", time_start_first_opt))
#   print(paste0("Start of the second optimization start: ", time_start_second_opt))
# }

# Save a version of rMatrix to see how much it changes.
rMatrix_first_opt <- rMatrix

# Export this version
write.table(rMatrix_first_opt, file = paste0(filepath_out, "rMatrix_first_opt_", "debug_flag_", run_debug_flag, "_", Sys.Date(), ".txt"), sep = " ",col.names = F,row.names = F)



##### SECOND OPTIMIZATION ROUTINE #####


# Put in flag to only run the code once
if (run_debug_flag == 1){
  max_iter_so = 1
} else {
  max_iter_so = 120
}


for (j in 1:max_iter_so){
  
  
  # Append the storage list
  log_storage_list[[count_var]] <- paste0("Column ", j, " in the second optimization routine started running at ", Sys.time())
  
  # Print the entry, if indicated
  if(print_debug_flag == 1){
    log_storage_list[[count_var]] %>% print()
  }
  
  # Update the count variable
  count_var <- count_var + 1
  
# 
#   if(print_debug_flag == 1){
#     print(paste0("Column ", j, " in the second optimization routine started running at ", Sys.time()))
#   }
  
    result = rMatrix[j,]
    
    R = Rall[,j]
    R2 = Rall2[,j]
    
    # Sets the values of g_1 to 0 if it's on the boundary of the parameter space. 
    # These are the same constraints as in the first routine.
    r11 = 0.001
    r12 = 0.999
    if (result[2] == 0||result[2]==0.001){
      r11 = 0
      r12 = 0
    }
    
    # Sets the values of b_1 to 0 if it's on the boundary of the parameter space. 
    # These are the same constraints as in the first routine.
    #b11 = 0.001
    #b12 = 0.999
    #if (result[8] == 0){
      b11 = 0
      b12 = 0
    #}
    
    # Sets the values of g_4 to 0 if it's on the boundary of the parameter space. 
    # The upper constraint has gone down from 40 to 20.
    r41 = 0.001
    r42 = 20
    if (result[5] == 0||result[5]==0.001){
      r41 = 0
      r42 = 0
    }
    
    # Sets the values of g_5 to 0 if it's on the boundary of the parameter space. 
    # The upper constraint has gone down from 40 to 20.
    r51 = exp(1)/min(R2)
    r52 = 20
    if (result[6] == 0||result[5]==0.001){
      r51 = 0
      r52 = 0
    }
    
    # Sets the values of g_5 to 0 if it's on the boundary of the parameter space. 
    # The upper constraint has gone down from 40 to 20.
    b41 = -50
    b42 = -0.001
    if (result[11] == 0||result[11]==-0.001){
      b41 = 0
      b42 = 0
    }
    
    # If b_5 is 0 or b_4 is on the upper end of the parameter space, then b_5 is automatically 0
    # The upper constraint has gone up from 40 to 250.
    b51 = exp(1)/min(R2)
    b52 = 250
    if (result[12] == 0||result[11]==-0.001){
      b51 = 0
      b52 = 0
    }
    
    # g = gamma, b = beta
    #    g_0, g_1,   g_2,           g_3, g_4, g_5, b_0, b_1,    b_2,              b_3, b_4, b_5,  theta
    lb=c(-10, r11, 0.001, exp(1)/min(R), r41, r51, -10, b11,   -200,    exp(1)/min(R), b41, b51,  0.001)
    ub=c( 10, r12,    80,          1000, r42, r52,  20, b12, -0.001,              800, b42, b52, min(R))
    
    source("Acf.R")
  
    rMatrix[j,] = result
}



# Record a time for the start of the maxima section
time_export <- Sys.time()


# Append the storage list
log_storage_list[[count_var]] <- paste0("Start of the export: ", time_export)

# Print the entry, if indicated
if(print_debug_flag == 1){
  log_storage_list[[count_var]] %>% print()
}

# Update the count variable
count_var <- count_var + 1

# 
# if(print_debug_flag == 1){
#   print(paste0("Start of the second optimization : ", time_start_second_opt))
#   print(paste0("Start of the export: ", time_export))
# }



# Modification on 11/14 - export the key results from yesterday

# NOTE: Make sure to update the path!
write.table(Rall, file = paste0(filepath_out, "Rall_", "debug_flag_", run_debug_flag, "_", Sys.Date(), ".txt"), sep = " ",col.names = F,row.names = F)
write.table(Rall2, file = paste0(filepath_out, "Rall2_", "debug_flag_", run_debug_flag, "_", Sys.Date(),  ".txt"), sep = " ",col.names = F,row.names = F)
write.table(rMatrix, file = paste0(filepath_out, "rMatrix_", "debug_flag_", run_debug_flag, "_", Sys.Date(),  ".txt"), sep = " ",col.names = F,row.names = F)


# ADJUSTMENT 4: Saves all the parameters in a rounded dataset
temp = round(rMatrix,3)
parameter = data.frame(temp[,-14])

# write.csv(parameter, file = paste0(filepath_out, "parameter3.csv"))
write.csv(parameter, file = paste0(filepath_out, "parameter3_", "debug_flag_", run_debug_flag, "_", Sys.Date(), ".csv"))

# Parameter info - these lines can be found directly from SimMLE.R
# gamma0=para[1];gamma1=para[2];gamma2=para[3];gamma3=para[4];gamma4=para[5];gamma5=para[6]
# beta0=para[7];beta1=para[8];beta2=para[9];beta3=para[10];beta4=para[11];beta5=para[12]

# theta[13] is mu

# Update the names of the columns
parameter_clean <- parameter

# Create three columns to match the index
Row <- rep(1:nrow_out, each=ncol_out)
Column <- rep(seq(1:ncol_out), nrow_out)
ID <- paste0("r", Row, "c", Column)

# Append these columns
parameter_clean <- cbind(ID, Row, Column, parameter_clean)

# Rename the columns
colnames(parameter_clean) <- c("ID", "Row", "Column",
                               "gamma_0", "gamma_1", "gamma_2", "gamma_3", "gamma_4", "gamma_5",
                               "beta_0", "beta_1", "beta_2", "beta_3", "beta_4", "beta_5", "mu")


# Export the file
# write.csv(parameter_clean, file = paste0(filepath_out, "parameter3_clean.csv"))
# Exclude row names
write.csv(parameter_clean, file = paste0(filepath_out, "parameter3_clean_", "debug_flag_", run_debug_flag, "_", Sys.Date(), ".csv"), row.names = FALSE)

# Count the end time
time_end=Sys.time()

delta = time_end - time_start


if (push_notification_flag == 1){
  pushover(message = paste0("The parameter finding code with debug flag equal to ", run_debug_flag,
                            " started running at ", time_start, 
                            " and it stopped running at ", time_end))
}



# Append the storage list
log_storage_list[[count_var]] <- paste0("This code started running at: ", time_start)

# Print the entry, if indicated
if(print_debug_flag == 1){
  log_storage_list[[count_var]] %>% print()
}

# Update the count variable
count_var <- count_var + 1

# Append the storage list
log_storage_list[[count_var]] <- paste0("This code finished running at: ", time_end)

# Print the entry, if indicated
if(print_debug_flag == 1){
  log_storage_list[[count_var]] %>% print()
}

# Update the count variable
count_var <- count_var + 1




# 
# if(print_debug_flag == 1){
#   print(paste0("This code started running at: ", time_start))
#   print(paste0("This code finished running at: ", time_end))
# }

# Export the list
if (export_log_flag == 1){
  sink(paste0(filepath_out, "code_log_", "debug_flag_", run_debug_flag, "_", Sys.Date(),  ".txt"))
  print(log_storage_list)
  sink()
  # capture.output(summary(log_storage_list), file = paste0(filepath_out, "code_log_", "debug_flag_", run_debug_flag, "_", Sys.Date(),  ".txt"))
}

# if (export_log_flag == 1){
#   capture.output(summary(log_storage_list), file = paste0(filepath_out, "code_log_", "debug_flag_", run_debug_flag, "_", Sys.Date(),  ".txt"))
# }




# write.table(rMatrix, file = paste0(filepath_out, "rMatrix_", "debug_flag_", run_debug_flag, "_", Sys.Date(),  ".txt"), sep = " ",col.names = F,row.names = F)



###### APPENDIX ######

# This was earlier code used to develop the visualization

# Calculate a num_images argument

# long_data <- data %>% pivot_longer(cols = starts_with("V"),
#                                    names_to = "Location",
#                                    names_prefix = "V",
#                                    values_to = "Brightness") %>% 
#   # Create a numeric location, and a group column
#   mutate(Location_Num = as.numeric(Location),
#          Location_Group = ceiling(Location_Num/10))
# 
# # Feed into the next thing
# long_data %>% mutate(Image_Number = rep(1:(nrow(long_data)/max(Location_Num)), each = max(Location_Num)),
#                      Image_ID = rep(seq(1:(max(Location_Num)/max(Location_Group))),(max(Location_Group))*(nrow(long_data)/max(Location_Num))),
#                      Image_ID_fact = as.factor(Image_ID)) %>% 
#   # Sort the data
#   arrange(Location_Num) %>% 
#   # filter(Location_Group == 1) %>% 
#   # Create a visualization
#   ggplot(aes(x = Image_Number, y = Brightness, color = Image_ID_fact, group = Image_ID_fact)) + 
#   geom_line(linetype = "dashed") + facet_wrap(vars(Location_Group)) +
#   labs(x = "Image Number", color = "Column")
# 
# 
# long_data %>% mutate(Image_Number = rep(1:(nrow(long_data)/max(Location_Num)), each = max(Location_Num))) %>% print(n = 125)



# Select below this line and uncomment to run the code as before.
# 
# 
# 
# 
# 
# # Modification on 11/16 - load the data back into the
# # Rall = read.table(file = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Run_11142022/Rall.txt")
# # Rall2 = read.table(file = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Run_11142022/Rall2.txt")
# # rMatrix = read.table(file = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Run_11142022/rMatrix.txt")
# 
# 
# # These select the 7 sub-areas to draw the graph
# # In these 7 areas, 3 include no solar flare, 2 include the first, and 2 include the second
# 
# ## 1, 2, and 12 are no solar flare
# ## 33 and 34 are the first flare
# ## 44 and 45 are the second flare
# 
# Rall_example = Rall[,c(1,2,12,33,34,44,45)]
# Rall2_example = Rall2[,c(1,2,12,33,34,44,45)]
# 
# # ADJUSTMENT - we only select the rows that we chose to analyze
# rMatrix_example299 = rMatrix[c(1,2,12,33,34,44,45),]
# # rMatrix_example299 = rMatrix[c(1,2,12,33,34,44,45),]
# 
# 
# ## We draw the plot of the alpha and the sigma to ID the plot
# 
# alphaexpAll_299=matrix(0,ncol=dim(Rall_example)[1],nrow=dim(Rall_example)[2])
# sigmaexpAll_299=matrix(0,ncol=dim(Rall_example)[1],nrow=dim(Rall_example)[2])
# for(i in 1:dim(Rall_example)[2]){
#   # ADJUSTMENT 3
#   # alphaexpAll_299[i,]=rec_alphaLog(rMatrix_example299[i,-14],Rall_example[,i],Rall2_example[,i])
#   sigmaexpAll_299[i,]=rec_sigma(rMatrix_example299[i,-14],Rall_example[,i],Rall2_example[,i])
# }
# 
# # ADJUSTMENT 4: Change to 299
# plot(sigmaexpAll_299[2,],type='l')
# plot(alphaexpAll_299[2,],type='l')
# 
# 
# # Modification on 11/14/2022
# 
# write.table(alphaexpAll_299, file = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Run_11142022/alphaAll_299_spatial.txt", sep = " ",col.names = F,row.names = F)
# write.table(sigmaexpAll_299, file = "/Users/stevenmoen/Documents/GitHub/solar_flare_project/Run_11142022/sigmaAll_299_spatial.txt", sep = " ",col.names = F,row.names = F)
# 
# 
# 
# # write.table(alphaexpAll_299, file = "alphaAll_299_spatial.txt", sep = " ",col.names = F,row.names = F)
# # write.table(sigmaexpAll_299, file = "sigmaAll_299_spatial.txt", sep = " ",col.names = F,row.names = F)
# 
# # ADJUSTMENT 4: Saves all the parameters in a rounded dataset
# temp = round(rMatrix,3)
# # temp = round(rMatrix_com,3)
# parameter = data.frame(temp[,-14])
# 
# write.csv(parameter,"/Users/stevenmoen/Documents/GitHub/solar_flare_project/Run_11142022/parameter3.csv")
# # write.csv(parameter,"parameter3.csv")
# 
# 
# 
# 
# 
# 
# 
