# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following path
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"

# Load in libraries
library(tidyverse)

# Read in 2011 original data
data_2011_old = read.table(paste0(your_path_to_data, "data.txt"),header = F)

##### Figure S3 #####

data_2011_old |> 
  mutate(`Time ID` = seq(1,299)) |> 
  # Pivot wide to long
  pivot_longer(cols = V1:V120, names_to = "Location ID", values_to = "Brightness Difference") |> 
  # Append row and column names
  mutate(Columns = rep(1:10, 12*299),
         Rows = rep(rep(1:12, each = 10), 299)) |> 
  ggplot(aes(x = `Time ID`, y = `Brightness Difference`)) +
  geom_line() + 
  facet_grid(vars(Rows), vars(Columns))
  # facet_wrap(~`Location ID`)

##### Table S1 #####
 
options(pillar.sigfig = 7)

data_2011_old |> 
  mutate(`Time ID` = seq(1,299)) |> 
  # Pivot wide to long
  pivot_longer(cols = V1:V120, names_to = "Location ID", values_to = "Brightness Difference") |> 
  # Append row and column names
  mutate(Columns = rep(1:10, 12*299),
         Rows = rep(rep(1:12, each = 10), 299)) |> 
  # print(n = 120)
  dplyr::filter(`Location ID` %in% c("V1", "V12", "V33", "V34", "V44", "V45")) |> 
  group_by(`Location ID`) |> 
  summarize(mean = mean(`Brightness Difference`),
            median = median(`Brightness Difference`),
            max = max(`Brightness Difference`),
            min = min(`Brightness Difference`),
            sd = sd(`Brightness Difference`)) 