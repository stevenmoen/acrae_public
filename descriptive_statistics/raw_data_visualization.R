# This code reproduces Figures S1 and S2

# It is recommended to clear your environment before running this code. Uncomment and run the line below.
# rm(list = ls())

# Update the following paths
your_path_to_data = "~/Documents/GitHub/acrae_public/data/"

# This reads the data into a named list
data <- readMat(paste0(your_path_to_data, "2011_Raw_Data/raw_2011_solar_data.mat"))

# Read in libraries
library(R.matlab)

# View the structure of your imported variables
str(data)
str(data$data)
# 
# image(t(data$data[,,215]), useRaster = TRUE, axes = FALSE, col = colorRampPalette(c("black", "red"))(256))
# 
# min(data$data[,,215])
# max(data$data[,,215])

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

##### FIGURE S5 #####
data$data[,,215]

