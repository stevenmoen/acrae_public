# This code reproduces Figure S37 and Table S14

# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following paths
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"

# Load in libaries
library(tidyverse)
library(ggtext)

# This is where you specify the data file - read in the rds file
data_2017_raw = readRDS(paste0(your_path_to_data, "Processed_Data/2017_Data_Para_Input_Files_New/max_df_2017_new_2026-02-16.rds"))
data_2022_raw = readRDS(paste0(your_path_to_data, "Processed_Data/2022_Data_Para_Input_Files_New/max_df_2022_new_2024-08-29.rds"))
data_2023_raw = readRDS(paste0(your_path_to_data, "Processed_Data/2023_Data_Para_Input_Files_New/max_df_2023_new_2024-08-30.rds"))

# Pivot each longer, then add a data index, a time index, and a split point
data_2017 <- data_2017_raw |> 
  mutate(run_id = "2017_flare",
    time_step = seq(1:nrow(data_2017_raw))) |> 
  pivot_longer(cols = V1:V120, names_to = "location", values_to = "max_abs_brightness_change") |> 
  mutate(after_split_id = ifelse(time_step > 232, 1, 0))

# Pivot each longer, then add a data index, a time index, and a split point
data_2022 <- data_2022_raw |> 
  mutate(run_id = "2022_flare",
         time_step = seq(1:nrow(data_2022_raw))) |> 
  pivot_longer(cols = V1:V120, names_to = "location", values_to = "max_abs_brightness_change") |> 
  mutate(after_split_id = ifelse(time_step > 550, 1, 0))

# Pivot each longer, then add a data index, a time index, and a split point
data_2023 <- data_2023_raw |> 
  mutate(run_id = "2023_flare",
         time_step = seq(1:nrow(data_2023_raw))) |> 
  pivot_longer(cols = V1:V120, names_to = "location", values_to = "max_abs_brightness_change") |> 
  mutate(after_split_id = ifelse(time_step > 494, 1, 0))

# Create a large dataset
data_all_periods <- rbind(data_2017, data_2022, data_2023)

# Create a breakpoint dataset
boxes <- tibble(
  run_id = c("2017_flare", "2022_flare", "2023_flare"),
  xmin = c(232+1, 550+1, 494+1)
)

##### Figure S37 #####

# Create a summary visualization
pre_and_post_peak_visualization = data_all_periods |> 
  group_by(run_id, time_step, after_split_id) |> 
  summarize(mean_mabc = mean(max_abs_brightness_change),
            median_mabc = median(max_abs_brightness_change),
            q1_mabc = quantile(max_abs_brightness_change, probs = 0.25),
            q3_mabc = quantile(max_abs_brightness_change, probs = 0.75)) |> 
            # sd_mabc = sd(max_abs_brightness_change)) |> 
  ggplot(aes(x = time_step, y = median_mabc)) +
    geom_line()+
    # Add the upper and lower quantiles
    geom_line(aes(y = q1_mabc), color = "#f03b20", lty = 2) + 
    geom_line(aes(y = q3_mabc), color = "#f03b20", lty = 2) + 
    geom_rect(
      data = boxes,
      aes(xmin = xmin, xmax = Inf, ymin = -Inf, ymax = Inf),
      alpha = 0.2,
      fill = "sandybrown",
      inherit.aes = FALSE
    ) + 
    facet_wrap(~run_id, labeller = as_labeller(c(
      "2017_flare" = "2017 Flare",
      "2022_flare" = "2022 Flare",
      "2023_flare" = "2023 Flare"
    ))) +
  labs(x = "Time", y = "Absolute Difference in Brightness",
       caption = paste0('1.&nbsp; The black line represents the median.<br>',
       '2.&nbsp; The lower and upper <span style="color:#f03b20">red dashed</span> lines represent the first and third quartiles, respectively.<br>',
       '3.&nbsp; The shaded regions represent the post-peak data.')) +
  theme_bw() + 
  theme(
    plot.caption = ggtext::element_markdown(hjust=0)
  ) + 
  scale_y_continuous(labels = scales::comma)

# Export the figure
# ggsave(filename="pre_and_post_peak.jpg", plot=pre_and_post_peak_visualization, 
#        path = "~/Documents/GitHub/solar_flare_project/Data_Cutting_for_Prediction_MR_R2", dpi = 600, height = 5, width = 8)

##### Table S14 #####

# Create a summary table
data_all_periods |> 
  group_by(run_id, after_split_id) |> 
  summarize(mean_mabc = mean(max_abs_brightness_change),
            sd_mabc = sd(max_abs_brightness_change))
