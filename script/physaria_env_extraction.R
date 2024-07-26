library(sf)
library(sp)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
# library(stars)

setwd("C:/Users/maial/Downloads/MBG REU/MOBOT_Physaria")

env_files <- list.files("./data/env_files", pattern = "*.tif$")
env_files

Physaria_data <- read_csv("./data/Physaria_MergedData_20240703.csv")

Physaria_data_sf <- st_as_sf(Physaria_data, coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)

plot(Physaria_data_sf)

wc_names <- c(BIO1 = "Annual Mean Temperature",
              BIO2 = "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
              BIO3 = "Isothermality (BIO2/BIO7) (×100)",
              BIO4 = "Temperature Seasonality (standard deviation ×100)",
              BIO5 = "Max Temperature of Warmest Month",
              BIO6 = "Min Temperature of Coldest Month",
              BIO7 = "Temperature Annual Range (BIO5-BIO6)",
              BIO8 = "Mean Temperature of Wettest Quarter",
              BIO9 = "Mean Temperature of Driest Quarter",
              BIO10 = "Mean Temperature of Warmest Quarter",
              BIO11 = "Mean Temperature of Coldest Quarter",
              BIO12 = "Annual Precipitation",
              BIO13 = "Precipitation of Wettest Month",
              BIO14 = "Precipitation of Driest Month",
              BIO15 = "Precipitation Seasonality (Coefficient of Variation)",
              BIO16 = "Precipitation of Wettest Quarter",
              BIO17 = "Precipitation of Driest Quarter",
              BIO18 = "Precipitation of Warmest Quarter",
              BIO19 = "Precipitation of Coldest Quarter")


usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()
canada_state <- ne_states(country = "canada", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()
mexico_state <- ne_states(country = "mexico", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

class(usa_state)
plot(usa_state, axes = TRUE)
plot(canada_state, axes = TRUE)
plot(mexico_state, axes = TRUE)


env_files[[19]]
env_layer9 <- raster(paste0("./data/env_files/", env_files[[19]]))

plot(env_layer9, xlim = c(-89, -84), ylim = c(36, 39), axes = TRUE)
points(Physaria_data$Longitude, Physaria_data$Latitude)

env_files[[18]]
env_layer8 <- raster(paste0("./data/env_files/", env_files[[18]]))

plot(env_layer8, xlim = c(-89, -84), ylim = c(36, 39), axes = TRUE)
points(Physaria_data$Longitude, Physaria_data$Latitude)

env_files[[17]]
env_layer <- raster(paste0("./data/env_files/", env_files[[17]]))

env_files[[1]]
env_layer <- raster(paste0("./data/env_files/", env_files[[1]]))

plot(env_layer9, xlim = c(-89, -84), ylim = c(36, 39), axes = TRUE)
points(Physaria_data$Longitude, Physaria_data$Latitude)

points(x = -75.06528, y = 38.61383)

plot(env_layer, xlim = c(-75.2, -74.8), ylim = c(38.5, 38.7), axes = TRUE)
points(x = -75.06528, y = 38.61383)

# env_files[[17]]

extracted_env_list <- list()

for (i in 1:length(env_files)) {
  env_layer <- raster(paste0("./data/env_files/", env_files[[i]])) 
  class(env_layer)
  crs(env_layer) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(env_layer)
  extracted_env <- raster::extract(env_layer, Physaria_data_sf, df = TRUE)
  
  extracted_env_list[[i]] <- as.tibble(extracted_env)[,2]
}

extracted_env_df <- bind_cols(extracted_env_list)

##############

extracted_buf_env_list <- list()

for (i in 1:length(env_files)) {
  env_layer <- raster(paste0("./data/env_files/", env_files[[i]])) 
  class(env_layer)
  crs(env_layer) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  crs(env_layer)
  extracted_env <- raster::extract(env_layer, Physaria_data_sf, buffer = 1000,fun = mean, na.rm = TRUE, df = TRUE)
  
  extracted_buf_env_list[[i]] <- as.tibble(extracted_env)[,2]
}

extracted_buf_env_df <- bind_cols(extracted_buf_env_list)

for (i in colnames(extracted_buf_env_df)){
  plot(extracted_buf_env_df[[i]], extracted_buf_env_df[[i]], main = i)
}

any(is.na(extracted_buf_env_df))
any(is.na(extracted_buf_env_df))



physaria_climate_data <- bind_cols(Physaria_data, extracted_buf_env_df)

write_csv(physaria_climate_data, "physaria_buf_climate_data_20240717.csv")
plot(env_layer)
class(env_layer)

physaria_climate_data_sp <- st_as_sf(physaria_climate_data, coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)

colnames(physaria_climate_data_sp) <- colnames(physaria_climate_data_sp) %>%
   str_remove("current_30arcsec_")%>%
   str_remove(".tif")

colnames(physaria_climate_data_sp)

range(physaria_climate_data$current_30arcsec_annualPET)
range(physaria_climate_data_sp$wc2.1_30s_bio_1)

for (i in colnames(physaria_climate_data_sp)[139:158]){
  print(i)
  env_layer_croped <- raster(paste0("./data/env_files/", i, ".tif"))%>%
    crop(extent(-89,-84,36,39))
  env_layer_df <- as.data.frame(env_layer_croped, xy=TRUE)
  
  
  p <- ggplot(data = physaria_climate_data_sp)+
    geom_raster(data = env_layer_df, aes(x=x, y=y,fill=get(i)))+
    geom_sf(data = usa_state, fill = NA)+
    geom_sf(size = 3)+
    scale_fill_distiller(palette = "YlGnBu") +
    ggtitle(i)+
    theme_bw()+
    theme(panel.grid = element_blank())+
    coord_sf(xlim = c(-89, -84), ylim = c(36, 39))
  ggsave(paste0("./figures/",i, ".png"), width = 16, height = 12)
  
}

crop(env_layer, extent(-125,-65,25,50))

setwd("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Genome_paper")

env_layer_croped <- crop(env_layer, extent(-125,-65,25,50))
plot(env_layer_croped)

env_layer_df <- as.data.frame(env_layer_croped, xy=TRUE)
env_layer_df <- env_layer_df[!is.na(env_layer_df$wc2.1_30s_bio_11),]
head(env_layer_df)

p <- ggplot(data = cyano_data_sf)+
  geom_raster(data = env_layer_df, aes(x=x, y=y,fill=wc2.1_30s_bio_11))+
  geom_sf(data = usa_state, fill = NA)+
  geom_sf(data = canada_state, fill = NA)+
  geom_sf(data = mexico_state, fill = NA)+
  geom_sf(size = 1, color = "yellow")+
  scale_fill_viridis_c(option = "inferno", name = "")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.key.height = unit(0.05, "in"), legend.key.width = unit(0.6, "in"), legend.margin = margin(t=-0.02, b=-0.02, unit = "in"), legend.position = "bottom")+
  xlab("")+
  ylab("")+
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE)

ggsave("bio19_sample_indiv2.png", width = 4, height = 3)
 