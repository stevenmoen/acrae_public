# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Load in libraries
library(tidyverse)
library(ggtext)

# Read in the data
s1000_NSF.r1c1_True <- c(1000, "NSF.r1c1", "True Value", -3.914, 0, 29.023, 15.782, 6.632, 21.873, 5.404, 0, 1.523, 0.022, 0, NA,102.532)
s1000_NSF.r1c1_Mean <- c(1000, "NSF.r1c1", "Mean", -3.689, 0,  27.676, 16.308,  6.358, 22.527,  5.341,  0,  1.415, 0.022,  0, NA, 101.682)
s1000_NSF.r1c1_SD <- c(1000, "NSF.r1c1", "SD",0.229, NA, 0.906, 1.481, 1.638, 1.544, 0.259, NA, 0.765, 0.010, NA, NA, 4.331)
s5000_NSF.r1c1_True <- c(5000, "NSF.r1c1", "True Value", -3.914, 0, 29.023, 15.782, 6.632, 21.873, 5.404, 0, 1.523, 0.022, 0, NA,102.532)
s5000_NSF.r1c1_Mean <- c(5000, "NSF.r1c1", "Mean",-3.849, 0, 28.312, 15.356, 6.681, 22.138, 5.406, 0, 1.583, 0.022, 0, NA,102.703)
s5000_NSF.r1c1_SD <- c(5000, "NSF.r1c1", "SD",0.147, NA, 0.197, 1.172, 1.252, 1.269, 0.139, NA, 0.441, 0.006, NA, NA, 1.702) 
# r2c2
s1000_NSF.r2c2_True <- c(1000, "NSF.r2c2", "True Value", -2.813, 0.197, 25.662, 25.552, 0.173, 0.016, 8.266, 0, 14.692, 0.298, 4.612, 15.557, 119.911)
s1000_NSF.r2c2_Mean <- c(1000, "NSF.r2c2", "Mean", -2.601, 0.171, 24.217, 26.016, 0.118, 0.016, 8.107, 0, 13.859, 0.294, 4.425, 15.447, 119.162)
s1000_NSF.r2c2_SD <- c(1000, "NSF.r2c2", "SD", 0.137, 0.076, 1.124, 1.300, 0.078, 0.005, 0.295, NA, 1.411, 0.071, 1.675, 1.382, 3.137)
s5000_NSF.r2c2_True <- c(5000, "NSF.r2c2", "True Value", -2.813, 0.197, 25.662, 25.552, 0.173, 0.016, 8.266, 0, 14.692, 0.298, 4.612, 15.557, 119.911)
s5000_NSF.r2c2_Mean <- c(5000, "NSF.r2c2", "Mean", -2.730, 0.195, 24.948, 25.146, 0.154, 0.017, 8.148, 0, 13.963, 0.295, 4.604, 15.462, 120.202)
s5000_NSF.r2c2_SD <- c(5000, "NSF.r2c2", "SD", 0.058, 0.048, 0.389, 1.113, 0.068, 0.004, 0.206, NA, 1.171, 0.063, 1.276, 1.107, 1.316)
# r4c3
s1000_SF1.r4c3_True <- c(1000, "SF1.r4c3", "True Value", -0.540, 0.023, 2.334, 0.025, 0, NA, 10.859, 0, 48.896, 23.956, 0.290, 0.007, 53.349)
s1000_SF1.r4c3_Mean <- c(1000, "SF1.r4c3", "Mean", -0.514, 0.023, 2.333, 0.026, 0, NA, 10.803, 0, 48.397, 24.298, 0.269, 0.007, 51.346)
s1000_SF1.r4c3_SD <- c(1000, "SF1.r4c3", "SD", 0.157, 0.010, 0.512, 0.005, NA, NA, 0.227, NA, 1.941, 1.776, 0.058, 0.001, 7.316)
s5000_SF1.r4c3_True <- c(5000, "SF1.r4c3", "True Value", -0.540, 0.023, 2.334, 0.025, 0, NA, 10.859, 0, 48.896, 23.956, 0.290, 0.007, 53.349)
s5000_SF1.r4c3_Mean <- c(5000, "SF1.r4c3", "Mean",-0.524, 0.023, 2.246, 0.025, 0, NA, 10.827, 0, 48.682, 24.050, 0.287, 0.007, 52.854)
s5000_SF1.r4c3_SD <- c(5000, "SF1.r4c3", "SD", 0.081, 0.009, 0.271, 0.002, NA, NA, 0.134, NA, 1.302, 1.668, 0.038, 0.001, 4.300)
# r4c4
s1000_SF1.r4c4_True <- c(1000, "SF1.r4c4", "True Value", -2.197, 0.097, 24.053, 65.603, 0, NA, 12.372, 0, 59.881, 69.024, 15.657, 59.961, 71.869)
s1000_SF1.r4c4_Mean <- c(1000, "SF1.r4c4", "Mean", -2.101, 0.088, 23.310, 65.194, 0, NA, 12.241, 0, 59.083, 70.458, 15.144, 59.171, 70.566)
s1000_SF1.r4c4_SD <- c(1000, "SF1.r4c4", "SD", 0.154, 0.019, 1.487, 2.617, NA, NA, 0.269, NA, 2.219, 1.819, 1.624, 3.819, 3.065)
s5000_SF1.r4c4_True <- c(5000, "SF1.r4c4", "True Value", -2.197, 0.097, 24.053, 65.603, 0, NA, 12.372, 0, 59.881, 69.024, 15.657, 59.961, 71.869)
s5000_SF1.r4c4_Mean <- c(5000, "SF1.r4c4", "Mean", -2.185, 0.092, 23.924, 65.300, 0, NA, 12.351, 0, 59.919, 70.058, 15.482, 60.139, 71.715)
s5000_SF1.r4c4_SD <- c(5000, "SF1.r4c4", "SD", 0.115, 0.018, 1.156, 2.272, NA, NA, 0.187, NA, 1.654, 1.774, 1.077, 3.117, 1.420)
# r5c4
s1000_SF2.r5c4_True <- c(1000, "SF2.r5c4", "True Value", -1.262, 0.180, 6.826, 0.105, 0, NA, 12.109, 0, 53.882, 50.633, 13.541, 45.522, 72.549)
s1000_SF2.r5c4_Mean <- c(1000, "SF2.r5c4", "Mean", -1.122, 0.177, 6.014, 0.090, 0, NA, 11.986, 0, 52.781, 50.236, 13.168, 45.186, 69.058)
s1000_SF2.r5c4_SD <- c(1000, "SF2.r5c4", "SD", 0.207, 0.069, 1.361, 0.037, NA, NA, 0.227, NA, 1.718, 1.877, 1.360, 1.779, 7.667)
s5000_SF2.r5c4_True <- c(5000, "SF2.r5c4", "True Value", -1.262, 0.180, 6.826, 0.105, 0, NA, 12.109, 0, 53.882, 50.633, 13.541, 45.522, 72.549)
s5000_SF2.r5c4_Mean <- c(5000, "SF2.r5c4", "Mean", -1.211, 0.182, 6.528, 0.101, 0, NA, 12.049, 0, 53.280, 50.343, 13.385, 45.214, 71.119)
s5000_SF2.r5c4_SD <- c(5000, "SF2.r5c4", "SD", 0.176, 0.039, 1.236, 0.034, NA, NA, 0.155, NA, 1.376, 1.771, 0.667, 1.741, 3.975)
# r5c5
s1000_SF2.r5c5_True <- c(1000, "SF2.r5c5", "True Value", -3.275, 0.074, 31.800, 9.025, 0.120, 0.013, 12.438, 0, 59.915, 72.122, 10.871, 21.867, 41.841)
s1000_SF2.r5c5_Mean <- c(1000, "SF2.r5c5", "Mean", -3.215, 0.070, 31.391, 9.072, 0.108, 0.013, 12.365, 0, 59.435, 72.684, 10.506, 20.887, 39.814)
s1000_SF2.r5c5_SD <- c(1000, "SF2.r5c5", "SD", 0.205, 0.010, 1.355, 1.567, 0.054, 0.005, 0.211, NA, 1.623, 1.434, 0.667, 1.381, 3.412)
s5000_SF2.r5c5_True <- c(5000, "SF2.r5c5", "True Value", -3.275, 0.074, 31.800, 9.025, 0.120, 0.013, 12.438, 0, 59.915, 72.122, 10.871, 21.867, 41.841)
s5000_SF2.r5c5_Mean <- c(5000, "SF2.r5c5", "Mean", -3.258, 0.074, 31.695, 9.062, 0.110, 0.013, 12.406, 0, 59.656, 72.503, 10.752, 21.070, 40.942)
s5000_SF2.r5c5_SD <- c(5000, "SF2.r5c5", "SD", 0.121, 0.009, 1.194, 1.304, 0.047, 0.004, 0.147, NA, 1.360, 1.434, 0.667, 1.381, 3.412)


# Combine into a data frame
table_2_data <- as.data.frame(rbind(s1000_NSF.r1c1_True, s1000_NSF.r1c1_Mean, s1000_NSF.r1c1_SD, s5000_NSF.r1c1_True, s5000_NSF.r1c1_Mean, s5000_NSF.r1c1_SD,
                                    s1000_NSF.r2c2_True, s1000_NSF.r2c2_Mean, s1000_NSF.r2c2_SD, s5000_NSF.r2c2_True, s5000_NSF.r2c2_Mean, s5000_NSF.r2c2_SD,
                                    s1000_SF1.r4c3_True, s1000_SF1.r4c3_Mean, s1000_SF1.r4c3_SD, s5000_SF1.r4c3_True, s5000_SF1.r4c3_Mean, s5000_SF1.r4c3_SD,
                                    s1000_SF1.r4c4_True, s1000_SF1.r4c4_Mean, s1000_SF1.r4c4_SD, s5000_SF1.r4c4_True, s5000_SF1.r4c4_Mean, s5000_SF1.r4c4_SD,
                                    s1000_SF2.r5c4_True, s1000_SF2.r5c4_Mean, s1000_SF2.r5c4_SD, s5000_SF2.r5c4_True, s5000_SF2.r5c4_Mean, s5000_SF2.r5c4_SD,
                                    s1000_SF2.r5c5_True, s1000_SF2.r5c5_Mean, s1000_SF2.r5c5_SD, s5000_SF2.r5c5_True, s5000_SF2.r5c5_Mean, s5000_SF2.r5c5_SD))
colnames(table_2_data) <- c("Sample Size", "Flare", "Statistic", "Gamma_0", "Gamma_1", "Gamma_2", "Gamma_3", "Gamma_4", "Gamma_5", "Beta_0", "Beta_1", "Beta_2", "Beta_3", "Beta_4", "Beta_5", "Mu")


#' A function to plot the parameters with error bars from the simulation
#'
#' @param parameter - the parameter in question. This is the name from the dataset. Something like Gamma_0
#' @param latex_parameter - the parameter specified in LaTeX, like $gamma_0$
#' @param df - the dataframe. Don't change this.
#' @param optional_caption - a caption if you'd like to mention something about the new parameters.
#' @param output_filepath - the output filepath. Don't change this.
#'
#' @returns - a plot
#' @export - a plot to the export location specified in output_filepath
#'
#' @examples - plot_parameters(parameter = "Gamma_5", latex_parameter = "$gamma_5^*$", optional_caption = paste0("<br>*There are no simulated values for SF1.r4c3, SF1.r4c4, or SF2.r5c4 as their associated gamma 4 values are zero."))
plot_parameters <- function(parameter, latex_parameter, df = table_2_data, optional_caption = "", output_filepath = "~/Documents/GitHub/solar_flare_project/Edited_Figures") {
  # Change variable types, changing some things to numeric and others to factors
  plot <- {{df}} %>% 
    mutate_at(c(1, 4:16), as.numeric) %>% 
    # Pivot longer, making one row per parameter
    pivot_longer(cols = Gamma_0:Mu,
                 names_to = "Parameter",
                 values_to = "Values") %>% 
    # Separate out true value, mean, and SD
    pivot_wider(names_from = "Statistic", 
                values_from = "Values") %>% 
    # Convert Parameter to a factor and relevel it
    mutate(Parameter = as.factor(Parameter)) %>% 
    mutate(Parameter=fct_relevel(Parameter,c("Gamma_0", "Gamma_1", "Gamma_2", "Gamma_3", "Gamma_4", "Gamma_5", 
                                             "Beta_0", "Beta_1", "Beta_2", "Beta_3", "Beta_4", "Beta_5", 
                                             "Mu"))) %>%
    # Arrange again, instead by location
    arrange(Parameter, Flare, `Sample Size`) %>% 
    # Look only at one parameter
    filter(Parameter == {{parameter}}) %>% 
    # Convert sample size to factor and recode factors
    mutate(`Sample Size` = as.factor(`Sample Size`),
           `Sample Size` = fct_recode(`Sample Size`, "1,000" = "1000", "5,000" = "5000")) %>%
    # This version uses a facet grid
    ggplot(aes(x = `Sample Size`, y = Mean)) +
    # Estimated parameter value
    geom_point() + 
    # Add true values
    geom_point(aes(y = `True Value`), color = "Red", shape = 4, size = 5) +
    # Add error bars
    geom_errorbar(aes(ymax = Mean + SD + 0.001, ymin = Mean - SD - 0.001),
                  position = "dodge")+ 
    # Add labels and a caption
    labs(x = "Flares and Sample Size", y = "Parameter Values", 
         # title = TeX("Simulation Results of $gamma_0"),
         title = TeX(paste0('Simulation Results of ', latex_parameter)),
         subtitle = "By Flare and Sample Size",
         caption = paste0("The black dot and error bars represent the sample mean and standard deviations of the MLEs obtained from 500 simulations.<br>The true parameter value is indicated with the <span style='color: red;'>X</span> mark.", optional_caption)) + 
    # "Mean and Std. Dev. are the sample mean and standard deviation of the MLE's obtained from 500 simulations.") + 
    facet_grid(~ Flare, switch = "x") +
    # Formatting the subtitle and caption
    theme(plot.subtitle=element_text(face='italic'),
          plot.caption = element_markdown(hjust=0))+
    # Format the plot
    theme(panel.spacing.x = grid::unit(0, "mm"),
          strip.placement = "outside",
          strip.background = element_blank()
    )
  # Print the plot
  print(plot)
  # Export the plot
  # ggsave(filename=paste0(parameter, "_simulation_plot.jpg"), plot=plot, path = output_filepath, dpi = 600)
}

# Call the above function
plot_parameters(parameter = "Gamma_0", latex_parameter = "$gamma_0$")
plot_parameters(parameter = "Gamma_1", latex_parameter = "$gamma_1$")
plot_parameters(parameter = "Gamma_2", latex_parameter = "$gamma_2$")
plot_parameters(parameter = "Gamma_3", latex_parameter = "$gamma_3$")
plot_parameters(parameter = "Gamma_4", latex_parameter = "$gamma_4$")
plot_parameters(parameter = "Gamma_5", latex_parameter = "$gamma_5^*$", optional_caption = paste0("<br>*There are no simulated values for SF1.r4c3, SF1.r4c4, or SF2.r5c4 as their associated gamma 4 values are zero."))
plot_parameters(parameter = "Beta_0", latex_parameter = "$beta_0$")
plot_parameters(parameter = "Beta_1", latex_parameter = "$beta_1$")
plot_parameters(parameter = "Beta_2", latex_parameter = "$beta_2$")
plot_parameters(parameter = "Beta_3", latex_parameter = "$beta_3$")
plot_parameters(parameter = "Beta_4", latex_parameter = "$beta_4$")
plot_parameters(parameter = "Beta_5", latex_parameter = "$beta_5^*$", optional_caption = "<br>*There are no simulated values for NSF.r1c1 since its associated beta 4 value is zero.")
plot_parameters(parameter = "Mu", latex_parameter = "$mu$")
