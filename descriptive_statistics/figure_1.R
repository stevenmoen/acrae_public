# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following path
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"

# Load in libraries
library(tidyverse)

# Read in 2011 original data
data_2011_old = read.table(paste0(your_path_to_data, "data.txt"),header = F)

# The six subareas are: NSF.r1c1, NSF.r2c2, SF1.r4c3, SF2.r5c4, SF1.r4c4, SF2.r5c5
# These correspond to: V1, V12, V33, V44, V34, V45

# Subset the data frame and plot
figure_3_revised <- data_2011_old %>% 
  # Add a time column
  mutate(Time = seq(1,299,1)) %>% 
  # Select relevant columns
  dplyr::select(Time, V1, V12, V33, V44, V34, V45) %>% 
  # Rename
  rename(NSF.r1c1 = V1, NSF.r2c2 = V12, SF1.r4c3 = V33, 
         SF2.r5c4 = V44, SF1.r4c4 = V34, SF2.r5c5 = V45) %>% 
  # Pivot longer
  pivot_longer(cols = `NSF.r1c1`:`SF2.r5c5`, 
               names_to = "Location",
               values_to = "Brightness") %>% 
  # Sort
  arrange(Location, Time) %>% 
  # This version specifies custom values below
  ggplot(aes(x = Time, y = Brightness, color = Location, linetype = Location)) +
  # Add lines
  geom_line() + 
  # Fix the theme
  theme_bw() +
  # Add labels
  labs(y = "Absolute Difference in Brightness")+
  # Add specific colors and linetypes
  scale_color_manual(values = c("#ffeda0", "#ffeda0", "#feb24c", "#feb24c", "#f03b20", "#f03b20")) + 
  scale_linetype_manual(values = rep(c(1,2),3)) + # This works fine in greyscale 02.23.2026
  scale_y_continuous(labels = scales::comma)

# Export the plot (revise as needed)
# ggsave(filename="fig3_revised.jpg", plot=figure_3_revised, path = "~/Documents/GitHub/solar_flare_project/Edited_Figures", dpi = 600, height = 4, width = 8)