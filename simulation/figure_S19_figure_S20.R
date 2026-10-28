# This code reproduces Figures S19 and S20

# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following paths
your_path_to_simulation = "~/Documents/GitHub/acrae_public/simulation/"
your_path_to_parameters = "~/Documents/GitHub/acrae_public/parameters/"
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"

# Load in libraries
library(VGAM)
library(tidyverse)

# The parameters should be saved in these subfolders
folder_file_2011_original_fs_parameter = "Original_Parameters_from_Supplement/Original_Parameters_from_Supplement.csv"

# Read in the parameters
parameters_2011_original_fs <- read.csv(file = paste0(your_path_to_parameters, folder_file_2011_original_fs_parameter), header = TRUE)
# Flip the sign
parameters_2011_original_fs$beta_2 <- parameters_2011_original_fs$beta_2*(-1)
parameters_2011_original_fs$beta_4 <- parameters_2011_original_fs$beta_4*(-1)

# Read in 2011 original data
data_old = read.table(paste0(your_path_to_data, "data.txt"),header = F)

# Generate 1,000 random Fréchet random variables
set.seed(227615)
r_frechet_draws <- rfrechet(n = 1000, shape = 1)

# Sort the vector
r_frechet_sort <- sort(r_frechet_draws)

# Load in the simulation file
source(paste0(your_path_to_simulation, "simulation_core.R"))

# Running this code can take a while. You can run it, or load in the .Rds file below
# simulated_data_original_paper_3000 <- extend_simul_with_max(input_param_df = parameters_2011_original_fs, r_seed_start = 445587, n_runs = 299, burn_in = 3000, use_empirical_frechet = 1)
# saveRDS(simulated_data_original_paper_3000, paste0(your_path_to_simulation, "simulated_data_original_paper_3000.Rds"))
simulated_data_original_paper_3000 = readRDS(paste0(your_path_to_simulation, "simulated_data_original_paper_3000.Rds"))

# Load in the code to produce Figures S19 and S20
source(paste0(your_path_to_simulation, "compute_viz_and_summary_stats_opt.R"))

##### Figure S19 #####
##### Figure S20 #####
compute_viz_and_summary_stats_opt(simulated_data = simulated_data_original_paper_3000, burn_in = 3000)
