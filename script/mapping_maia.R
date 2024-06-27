# Load necessary libraries

library(tidyverse)     # For data manipulation and visualization

install.packages("cowplot")
library(cowplot)       # For combining multiple plots

install.packages("sf")
library(sf)            # For handling spatial data

install.packages("rnaturalearth")
library(rnaturalearth) # For accessing natural earth data

library(rnaturalearthdata)
library(scatterpie)    # For creating scatter pie plots
library(raster)        # For raster data manipulation
library(ggspatial)     # For spatial data visualization

# Set working directory
setwd("./clover_script")

# Read input data
pca_data_mean <- read_csv("./data/figure_1_cyanotypes.csv")
pca_data_mean_pie_3857 <- read_csv("./data/figure_1_plotting.csv")

# Download this file from https://envirem.github.io/, which is not available on GitHub
env_layer <- raster("./data/current_30arcsec_growingDegDays5.tif")

# Get and crop country boundaries
usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()