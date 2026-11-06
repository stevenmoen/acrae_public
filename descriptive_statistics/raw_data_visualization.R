# This code reproduces Figures S1, S2, and S5

# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following paths
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"

# Read in libraries
library(R.matlab)
library(ggplot2)
library(tidyverse)

# This reads the data into a named list
data <- readMat(paste0(your_path_to_data, "Processed_Data/2011_Raw_Data/raw_2011_solar_data.mat"))
data_2011_old = read.table(paste0(your_path_to_data, "data.txt"),header = F)

##### Figure S1 #####
r1c1 <- data$data[,,186]
r1c1 <- t(r1c1[nrow(r1c1):1, ])
r1c2 <- data$data[,,198]
r1c2 <- t(r1c2[nrow(r1c2):1, ])
r1c3 <- data$data[,,202]
r1c3 <- t(r1c3[nrow(r1c3):1, ])
# Second row
r2c1 <- abs(data$data[,,186] - data$data[,,185])
r2c1 <- t(r2c1[nrow(r2c1):1, ])
r2c2 <- abs(data$data[,,198] - data$data[,,197])
r2c2 <- t(r2c2[nrow(r2c2):1, ])
r2c3 <- abs(data$data[,,202] - data$data[,,201])
r2c3 <- t(r2c3[nrow(r2c3):1, ])

# Red scale
par(mfrow = c(2,3))
image(r1c1, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r1c2, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r1c3, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r2c1, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r2c2, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r2c3, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))

##### Figure S2 #####
r1c1 <- data$data[,,215]
r1c1 <- t(r1c1[nrow(r1c1):1, ])
r1c2 <- data$data[,,230]
r1c2 <- t(r1c2[nrow(r1c2):1, ])
r1c3 <- data$data[,,268]
r1c3 <- t(r1c3[nrow(r1c3):1, ])
# Second row
r2c1 <- abs(data$data[,,215] - data$data[,,214])
r2c1 <- t(r2c1[nrow(r2c1):1, ])
r2c2 <- abs(data$data[,,230] - data$data[,,229])
r2c2 <- t(r2c2[nrow(r2c2):1, ])
r2c3 <- abs(data$data[,,268] - data$data[,,267])
r2c3 <- t(r2c3[nrow(r2c3):1, ])

# Red scale
par(mfrow = c(2,3))
image(r1c1, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r1c2, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r1c3, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r2c1, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r2c2, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
image(r2c3, useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))

# Prep for Figure S5
# 232 rows, 292 columns
# 232/12
# 228/12 # Let's assume the last 4 rows don't matter, rounding down to 228
# 292/10 # Assume the last 2 columns don't matter, rounding down to 290 rows

# 228/12 19 rows of data per group
# 290/10 29 columns of data per groups

# r4c3 example pull. Take everything from right after 3rd row to 4th row, everything right after 2nd column to end of 3rd 
# data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),215]
# r4c4
# data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),215]
# r5c3
# data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),215]
# r5c4
# data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),215]
# r5c5
# data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),215]

# Create storage vectors for the borders
r4c3_right_border <- rep(NA, 299*19)
r4c4_left_border <- rep(NA, 299*19)
r4c4_bottom_border <- rep(NA, 299*29)
r5c4_top_border <- rep(NA, 299*29)
r5c3_right_border <- rep(NA, 299*19)
r5c4_left_border <- rep(NA, 299*19)
r5c4_right_border <- rep(NA, 299*19)
r5c5_left_border <- rep(NA, 299*19)

# Loop through and populate
for (i in 1:299){
  # r4c3 right vs. r4c4 left
  # Line directly below says to take all rows but only the last column from adjacent time periods, then difference them
  r4c3_right_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(3*19 + 1):(4*19),(3*29),i+1] - data$data[(3*19 + 1):(4*19),(3*29),i])
  r4c4_left_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(3*19 + 1):(4*19),(3*29 + 1),i+1] - data$data[(3*19 + 1):(4*19),(3*29 + 1),i])
  # r4c4 bottom vs. r5c4 top
  r4c4_bottom_border[((i-1)*29 + 1):(i*29)] <- abs(data$data[(4*19),(3*29 + 1):(4*29),i+1] - data$data[(4*19),(3*29 + 1):(4*29),i])
  r5c4_top_border[((i-1)*29 + 1):(i*29)] <- abs(data$data[(4*19 + 1),(3*29 + 1):(4*29),i+1] - data$data[(4*19 + 1),(3*29 + 1):(4*29),i])
  # r5c3 right vs. r5c4 left
  r5c3_right_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(3*29),i+1] - data$data[(4*19 + 1):(5*19),(3*29),i])
  r5c4_left_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(3*29 + 1),i+1] - data$data[(4*19 + 1):(5*19),(3*29 + 1),i])
  # r5c4 right vs. r5c5 left
  r5c4_right_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(4*29),i+1] - data$data[(4*19 + 1):(5*19),(4*29),i])
  r5c5_left_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(4*29 + 1),i+1] - data$data[(4*19 + 1):(5*19),(4*29 + 1),i])
}

# Check for NAs
# sum(is.na(r4c3_right_border))
# sum(is.na(r4c4_left_border))
# sum(is.na(r4c4_bottom_border))
# sum(is.na(r5c4_top_border))
# sum(is.na(r5c3_right_border))
# sum(is.na(r5c4_left_border))
# sum(is.na(r5c4_right_border))
# sum(is.na(r5c5_left_border))

# Plot the border data
##### Figure S5 #####
par(mfrow = c(2,2))
plot(r4c3_right_border, r4c4_left_border, xlab = "SF1.r4c3", ylab = "SF1.r4c4")
plot(r4c4_bottom_border, r5c4_top_border, xlab = "SF1.r4c4", ylab = "SF2.r5c4")
plot(r5c3_right_border, r5c4_left_border, xlab = "NSF.r5c3", ylab = "SF2.r5c4")
plot(r5c4_right_border, r5c5_left_border, xlab = "SF2.r5c4", ylab = "SF2.r5c5")