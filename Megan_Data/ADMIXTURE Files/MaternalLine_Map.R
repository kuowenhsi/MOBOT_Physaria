

####################################################################################
########################### Simple Maternal Line Map ###############################
####################################################################################

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

setwd("~/Desktop/Physaria_Data/VCF-Plink-Remove-Admixture/ADMIXTURE_data_from_wen/Q_files")

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

#### 4) Summarizing unique maternal lines by county ####
unique(metadata$MaternalLine)
unique_lines_counties <- metadata %>%
  select(MaternalLine, County) %>%   
  distinct() %>%                     
  arrange(County, MaternalLine)
print(unique_lines_counties)

#### 5) Manually set name locations ####
maternal_locations <- maternal_locations %>%
  mutate(
    nudge_x = case_when(
      MaternalLine == "2021-1841-1" ~ 0.3,   
      MaternalLine == "2021-1843-2" ~ -0.3,  
      MaternalLine == "2021-1836-2" ~ -0.3, 
      MaternalLine == "2021-1839-2" ~ -0.3, 
      MaternalLine == "2021-1838-2" ~ 0.3, 
      TRUE ~ 0
    ),
    nudge_y = case_when(
      MaternalLine == "2021-1841-1" ~ -0.08,   
      MaternalLine == "2021-1843-2" ~ -0.08,   
      MaternalLine == "2021-1836-2" ~ -0.08,
      MaternalLine == "2021-1839-2" ~ -0.05, 
      MaternalLine == "2021-1838-2" ~ -0.08, 
      TRUE ~ 0
    )
  )

#### 6) Plot ####
q <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_point(data = maternal_locations,
             aes(x = Longitude, y = Latitude, color = County),
             size = 3) +
  geom_text(data = maternal_locations,
            aes(x = Longitude + nudge_x, y = Latitude + nudge_y, label = MaternalLine),
            size = 3, vjust = -1, color = "black") +
  geom_sf(data = rivers_cropped,
          color = "dodgerblue3",
          size = 2,
          alpha = 0.3,
          inherit.aes = FALSE) +
  coord_sf(xlim = c(-88.5, -84), ylim = c(36, 38.5), expand = FALSE) +
  theme_bw()

q

ggsave("MaternalLineMap.png", q,
       width = 6, height = 5, dpi = 300, units = "in", bg = "white")
