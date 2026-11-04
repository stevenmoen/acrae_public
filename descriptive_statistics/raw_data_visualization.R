# This code reproduces Figures S1 and S2

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


# View the structure of your imported variables
str(data$data)


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
# data$data[,,215]

# r4c3 vs. r4c4
# r4c4 vs. r5c4
# r5c3 vs. r5c4
# r5c4 vs. r5c5

data$data[,,215]
# 232 rows, 292 columns
# 232/12
# 228/12 # Let's assume the last 4 rows don't matter
# 292/10 # Assume the last 2 columns don't matter

# 228/12 19 rows of data per group
# 290/10 29 columns of data per groups

# r4c3
data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),215]
# as.vector(data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),215])
# r4c4
data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),215]
# r5c3
data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),215]
# r5c4
data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),215]
# r5c5
data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),215]


# Populate a big data frame by first populating each vector
# 300 images, 19 rows per group, 29 columns per group
# This works for raw data, undifferenced. 
# r4c3 <- rep(NA, 300*19*29)
# r4c4 <- rep(NA, 300*19*29)
# r5c3 <- rep(NA, 300*19*29)
# r5c4 <- rep(NA, 300*19*29)
# r5c5 <- rep(NA, 300*19*29)

# for (i in 1:300){
#   r4c3[((i-1)*19*29 + 1):(i*19*29)] <- data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),i] |> as.vector()
#   r4c4[((i-1)*19*29 + 1):(i*19*29)] <- data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),i] |> as.vector()
#   r5c3[((i-1)*19*29 + 1):(i*19*29)] <- data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),i] |> as.vector()
#   r5c4[((i-1)*19*29 + 1):(i*19*29)] <- data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),i] |> as.vector()
#   r5c5[((i-1)*19*29 + 1):(i*19*29)] <- data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),i] |> as.vector()
# }
# 
# # Combine
# big_df = data.frame(r4c3, r4c4, r5c3, r5c4, r5c5)
# 
# big_df |>
#   ggplot(aes(x = r4c3, y = r4c4)) +
#   geom_point()
# 
# big_df |>
#   ggplot(aes(x = r4c4, y = r5c4)) +
#   geom_point()

# min(r4c3)

# Try for the absolute differenced raw data
r4c3 <- rep(NA, 299*19*29)
r4c4 <- rep(NA, 299*19*29)
r5c3 <- rep(NA, 299*19*29)
r5c4 <- rep(NA, 299*19*29)
r5c5 <- rep(NA, 299*19*29)
# Populate a mean vector
r4c3_mean <- rep(NA, 299)
r4c4_mean <- rep(NA, 299)
r5c3_mean <- rep(NA, 299)
r5c4_mean <- rep(NA, 299)
r5c5_mean <- rep(NA, 299)
# Compute the borders?
r4c3_right_border <- rep(NA, 299*19)
r4c4_left_border <- rep(NA, 299*19)
r4c4_bottom_border <- rep(NA, 299*29)
r5c4_top_border <- rep(NA, 299*29)
r5c3_right_border <- rep(NA, 299*19)
r5c4_left_border <- rep(NA, 299*19)
r5c4_right_border <- rep(NA, 299*19)
r5c5_left_border <- rep(NA, 299*19)

for (i in 1:299){
  r4c3[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),i+1] - data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),i])
  r4c4[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),i+1] - data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),i])
  r5c3[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),i+1] - data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),i])
  r5c4[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),i+1] - data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),i])
  r5c5[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),i+1] - data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),i])
  # r4c3 right vs. r4c4 left
  r4c3_right_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(3*19 + 1):(4*19),(3*29),i+1] - data$data[(3*19 + 1):(4*19),(3*29),i])
  r4c4_left_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(3*19 + 1):(4*19),(3*29 + 1),i+1] - data$data[(3*19 + 1):(4*19),(3*29 + 1),i])
  # r4c4 bottom vs. r5c4 top
  r4c4_bottom_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19),(3*29 + 1):(4*29),i+1] - data$data[(4*19),(3*29 + 1):(4*29),i])
  r5c4_top_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1),(3*29 + 1):(4*29),i+1] - data$data[(4*19 + 1),(3*29 + 1):(4*29),i])
  # r5c3 right vs. r5c4 left
  r5c3_right_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(3*29),i+1] - data$data[(4*19 + 1):(5*19),(3*29),i])
  r5c4_left_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(3*29 + 1),i+1] - data$data[(4*19 + 1):(5*19),(3*29 + 1),i])
  # r5c4 right vs. r5c5 left
  r5c4_right_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(4*29),i+1] - data$data[(4*19 + 1):(4*19),(3*29),i])
  r5c5_left_border[((i-1)*19 + 1):(i*19)] <- abs(data$data[(4*19 + 1):(5*19),(4*29 + 1),i+1] - data$data[(4*19 + 1):(4*19),(3*29 + 1),i])
  # Store data
  r4c3_mean[i] <- mean(r4c3[((i-1)*19*29 + 1):(i*19*29)])
  r4c4_mean[i] <- mean(r4c4[((i-1)*19*29 + 1):(i*19*29)])
  r5c3_mean[i] <- mean(r5c3[((i-1)*19*29 + 1):(i*19*29)])
  r5c4_mean[i] <- mean(r5c4[((i-1)*19*29 + 1):(i*19*29)])
  r5c5_mean[i] <- mean(r5c5[((i-1)*19*29 + 1):(i*19*29)])
}

# Plot the border data
# plot(r4c3_border, r4c4_border, xlim = c(0, 1000), ylim = c(0, 1000))
##### Figure S5 #####
par(mfrow = c(2,2))
plot(r4c3_right_border, r4c4_left_border, xlab = "SF1.r4c3", ylab = "SF1.r4c4")
plot(r4c4_bottom_border, r5c4_top_border, xlab = "SF1.r4c4", ylab = "SF1.r5c4")
plot(r5c3_right_border, r5c4_left_border, xlab = "SF1.r5c3", ylab = "SF1.r5c4")
plot(r5c4_right_border, r5c5_left_border, xlab = "SF1.r5c4", ylab = "SF1.r5c5")

# plot(r4c3_right_border, r4c4_left_border, col = adjustcolor("blue", alpha.f = 0.2))


par(mfrow = c(2,2))
plot(r4c4_left_border ~ r4c3_right_border)
loess_fit <- loess(r4c4_left_border ~ r4c3_right_border)
# Predict smooth values and add them with lines()
# x_order <- order(r4c4_left_border)
# lines(mydata$x[x_order], predict(loess_fit)[x_order], col = "red", lwd = 2)

plot(r5c4_top_border ~ r4c4_bottom_border)
plot(r5c4_left_border ~ r5c3_right_border)
plot(r5c5_left_border ~ r5c4_right_border)

# y_vs_x_mod = lm(r4c4_border ~ r4c3_border)
# abline(y_vs_x_mod, col = "red")
# Plot the border data subsetted
plot(r4c3_border[(185*19):(201*19)], r4c4_border[(185*19):(201*19)], xlim = c(0, 1000), ylim = c(0, 1000))
# for (i in 185:201){

summary(r4c3_border)

r4c3_border[3515:3518]

par(mfrow = c(1,1))
plot(r4c3_mean, r4c4_mean)

# Combine
big_df = data.frame(r4c3, r4c4, r5c3, r5c4, r5c5)

big_df |>
  ggplot(aes(x = r4c3, y = r4c4)) +
  geom_point()

par(mfrow = c(2,2))
plot(big_df$r4c3, big_df$r4c4, xlim = c(0, 1000), ylim = c(0, 1000))
plot(big_df$r4c4, big_df$r5c4, xlim = c(0, 1000), ylim = c(0, 1000))


# big_df$r4c3

# Try with the raw data
par(mfrow = c(2,2))
plot(data_2011_old$V33, data_2011_old$V34, xlim = c(0, 1000), ylim = c(0, 1000))
plot(data_2011_old$V34, data_2011_old$V44, xlim = c(0, 1000), ylim = c(0, 1200))
plot(data_2011_old$V43, data_2011_old$V44, xlim = c(0, 800), ylim = c(0, 1200))
plot(data_2011_old$V44, data_2011_old$V45, xlim = c(0, 1200), ylim = c(0, 1000))
# data_2011_old |> 
#   ggplot(aes(x = r4c3, y = r4c4)) +
#   geom_point()

plot()

# data$r4c3# data$data

# What do we know for sure?
# It's definitely differenced. (The undifferenced scales are too small.)
# It's not every period.

# Try for the absolute differenced raw data
r4c3_cut <- rep(NA, 299*19*29)
r4c4_cut <- rep(NA, 299*19*29)
r5c3_cut <- rep(NA, 299*19*29)
r5c4_cut <- rep(NA, 299*19*29)
r5c5_cut <- rep(NA, 299*19*29)

r4c3_border <- rep(NA, 299*19)
r4c4_border <- rep(NA, 299*19)

par(mfrow = c(1,1))
# for (i in c(197, 229)){
# for (i in 1:299){
for (i in 185:201){
  # par(mfrow = c(5,5))
  par(mfrow = c(1,1))
  
  r4c3_cut[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),i+1] - data$data[(3*19 + 1):(4*19),(2*29 + 1):(3*29),i])
  r4c4_cut[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),i+1] - data$data[(3*19 + 1):(4*19),(3*29 + 1):(4*29),i])
  r5c3_cut[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),i+1] - data$data[(4*19 + 1):(5*19),(2*29 + 1):(3*29),i])
  r5c4_cut[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),i+1] - data$data[(4*19 + 1):(5*19),(3*29 + 1):(4*29),i])
  r5c5_cut[((i-1)*19*29 + 1):(i*19*29)] <- abs(data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),i+1] - data$data[(4*19 + 1):(5*19),(4*29 + 1):(5*29),i])
  plot(r4c3_cut[((i-1)*19*29 + 1):(i*19*29)], r4c4_cut[((i-1)*19*29 + 1):(i*19*29)], xlim = c(0, 1000), ylim = c(0,1000), 
       main = paste0("Period ", i))
  # Store data
  # r4c3_mean[i] <- mean(r4c3[((i-1)*19*29 + 1):(i*19*29)])
  # r4c4_mean[i] <- mean(r4c4[((i-1)*19*29 + 1):(i*19*29)])
  # r5c3_mean[i] <- mean(r5c3[((i-1)*19*29 + 1):(i*19*29)])
  # r5c4_mean[i] <- mean(r5c4[((i-1)*19*29 + 1):(i*19*29)])
  # r5c5_mean[i] <- mean(r5c5[((i-1)*19*29 + 1):(i*19*29)])
}

