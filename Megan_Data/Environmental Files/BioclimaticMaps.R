

####################################################################################
##################################### Bioclimatic ####################################
####################################################################################

library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyterra) 
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tigris)

setwd("~/Desktop/Physaria_Data/Fst_Stuff/byEnvironmental")

# Read in metadata
obs_data <- read.csv("Physaria_MergedData_20240703.csv")

# If necessary, ensure lat/lon are numeric
obs_data$Latitude <- as.numeric(obs_data$Latitude)
obs_data$Longitude <- as.numeric(obs_data$Longitude)

# Check data
summary(obs_data)

# Determine geographic extent of the points
max_lat <- ceiling(max(obs_data$Latitude, na.rm = TRUE))
min_lat <- floor(min(obs_data$Latitude, na.rm = TRUE))
max_lon <- ceiling(max(obs_data$Longitude, na.rm = TRUE))
min_lon <- floor(min(obs_data$Longitude, na.rm = TRUE))
geographic_extent <- ext(c(min_lon, max_lon, min_lat, max_lat))

# Optional: expand extent by 25%
sample_extent <- geographic_extent * 1.25

######################################### Bio17 ###################################

bio17 <- rast("wc2.1_30s_bio_17.tif")
bio17_cropped <- crop(bio17, sample_extent)

# Set up device (e.g., PNG with high resolution)
png("bio17_map.png", width = 1200, height = 1000, res = 150)

# Plot raster with terra::plot()
plot(bio17_cropped, main = "Bio17: Precipitation of Driest Quarter")

# Overlay sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

# Close the device
dev.off()

######################################### Bio14 ###################################

bio14 <- rast("wc2.1_30s_bio_14.tif")
bio14_cropped <- crop(bio14, sample_extent)

# Set up device (e.g., PNG with high resolution)
png("bio14_map.png", width = 1200, height = 1000, res = 150)

# Plot raster with terra::plot()
plot(bio14_cropped, main = "Bio14: Precipitation of Driest Month")

# Overlay sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

# Close the device
dev.off()

######################################### Bio12 ###################################

bio12 <- rast("wc2.1_30s_bio_12.tif")
bio12_cropped <- crop(bio12, sample_extent)

# Set up device (e.g., PNG with high resolution)
png("bio12_map.png", width = 1200, height = 1000, res = 150)

# Plot the raster
plot(bio12_cropped, main = "Bio12: Annual Precipitation")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

# Close the device
dev.off()

######################################### Bio19 ###################################
# difference
bio19 <- rast("wc2.1_30s_bio_19.tif")
bio19_cropped <- crop(bio19, sample_extent)

# Set up device (e.g., PNG with high resolution)
png("bio19_map.png", width = 1200, height = 1000, res = 150)

# Plot the raster
plot(bio19_cropped, main = "Bio19: Precipitation of Coldest Quarter")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

# Close the device
dev.off()

######################################### Bio11 ###################################
# big difference
bio11 <- rast("wc2.1_30s_bio_11.tif")
bio11_cropped <- crop(bio11, sample_extent)

# Set up device (e.g., PNG with high resolution)
png("bio11_map.png", width = 1200, height = 1000, res = 150)

# Plot the raster
plot(bio11_cropped, main = "Bio11: Mean Temp of Coldest Quarter")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

# Close the device
dev.off()

######################################### Bio4 ###################################
# difference
bio4 <- rast("wc2.1_30s_bio_4.tif")
bio4_cropped <- crop(bio4, sample_extent)

# Plot the raster
plot(bio4_cropped, main = "Bio4: Temperature Seasonality")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio3 ###################################
# difference
bio3 <- rast("wc2.1_30s_bio_3.tif")
bio3_cropped <- crop(bio3, sample_extent)

# Plot the raster
plot(bio3_cropped, main = "Bio3: Isothermality")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio13 ##################################

bio13 <- rast("wc2.1_30s_bio_13.tif")
bio13_cropped <- crop(bio13, sample_extent)

# Plot the raster
plot(bio13_cropped, main = "Bio13: Precipitation of Wettest Month")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio15 ##################################
# big difference
bio15 <- rast("wc2.1_30s_bio_15.tif")
bio15_cropped <- crop(bio15, sample_extent)

# Plot the raster
plot(bio15_cropped, main = "Bio15: Precipitation Seasonality")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio6 ###################################
# big difference
bio6 <- rast("wc2.1_30s_bio_6.tif")
bio6_cropped <- crop(bio6, sample_extent)

# Plot the raster
plot(bio6_cropped, main = "Bio6: Min Temperature of Coldest Month")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio16 ###################################

bio16 <- rast("wc2.1_30s_bio_16.tif")
bio16_cropped <- crop(bio16, sample_extent)

# Plot the raster
plot(bio16_cropped, main = "Bio16: Precipitation of Wettest Quarter")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio2 ###################################

bio2 <- rast("wc2.1_30s_bio_2.tif")
bio2_cropped <- crop(bio2, sample_extent)

# Plot the raster
plot(bio2_cropped, main = "Bio2: Mean Diurnal Range")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio1 ###################################

bio1 <- rast("wc2.1_30s_bio_1.tif")
bio1_cropped <- crop(bio1, sample_extent)

# Plot the raster
plot(bio1_cropped, main = "Bio1: Annual Mean Temperature")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

######################################### Bio7 ###################################

bio7 <- rast("wc2.1_30s_bio_7.tif")
bio7_cropped <- crop(bio7, sample_extent)

# Plot the raster
plot(bio7_cropped, main = "Bio7: Temperature Annual Range")

# Add sample points
points(obs_data$Longitude, obs_data$Latitude, 
       col = "white", pch = 20, cex = 0.75)

