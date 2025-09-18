

####################################################################################
########################### Simple Maternal Line Map ###############################
####################################################################################

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tigris)

setwd("~/Desktop/Physaria_Data/VCF-Plink-Remove-Admixture/Admixture_files")

#### 1) Load in meta data ####
metadata <- read.csv("Physaria_MergedData_20240703.csv")

#### 2) Locations of MaternalLines ####
maternal_locations <- metadata %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  group_by(MaternalLine) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    County = first(County)
  )

#### 3) Get map and river data ####
us_map <- map_data("state")
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
rivers_cropped <- st_crop(rivers, xmin = -84, ymin = 36, xmax = -88.5, ymax = 38.5)

#### 4) County outlines ####
counties_sf <- counties(state = c("TN", "KY", "IN"), cb = TRUE, year = 2021, class = "sf")
counties_cropped <- st_crop(counties_sf, xmin = -88.5, ymin = 36, xmax = -84, ymax = 38.5)

#### 5) Plot ####
q <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_sf(data = counties_cropped,
          fill = NA,
          color = "grey75",
          size = 0.4) + 
  geom_point(data = maternal_locations,
             aes(x = Longitude, y = Latitude),
             fill = "#FF61CC",
             color = "black",
             shape = 21,
             size = 3,
             stroke = 0.5) +
  geom_sf(data = rivers_cropped,
          color = "dodgerblue3",
          size = 2,
          alpha = 0.7,
          inherit.aes = FALSE) +
  coord_sf(xlim = c(-88.5, -84), ylim = c(36, 38.5), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0.8, 0, 0.8), "lines"))  

q

ggsave("MaternalLineMap.png", q,
       width = 6, height = 5, dpi = 300, units = "in", bg = "white")
