# Load necessary libraries
library(tidyverse)     # For data manipulation and visualization
# library(cowplot)       # For combining multiple plots
library(sf)            # For handling spatial data
library(rnaturalearth) # For accessing natural earth data
library(rnaturalearthdata)
# library(scatterpie)    # For creating scatter pie plots
# library(raster)        # For raster data manipulation
library(ggspatial)     # For spatial data visualization
library(ggrepel)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Physaria")

env_files <- list.files("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Undergrad/Grace/Env_varibles", pattern = "*.tif$")
env_files

Physaria_data <- read_csv("./data/Physaria_MergedData_20240703.csv")

Physaria_data_sf <- st_as_sf(Physaria_data, coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)

Physaria_data_label <- Physaria_data %>%
  group_by(MaternalLine) %>%
  summarise(Longitude = mean(Longitude), Latitude = mean(Latitude))%>%
  ungroup()%>%
  st_as_sf(coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)%>%
  st_transform(crs = 3857)

Physaria_data_label_3857 <- cbind(st_drop_geometry(Physaria_data_label), st_coordinates(Physaria_data_label))

# Get and crop country boundaries
usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = 0, xmax = -65, ymax = 50) %>%
  st_geometry()

canada_state <- ne_states(country = "canada", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = 0, xmax = -65, ymax = 50) %>%
  st_geometry()

mexico_state <- ne_states(country = "mexico", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = 0, xmax = -65, ymax = 50) %>%
  st_geometry()

# load Physaria env extracts

Physaria_env <- read_csv("./data/Physaria_buf_climate_data_20240724.csv")
Physaria_env_MaternalLine <- Physaria_env%>%
  select(-Index) %>%
  group_by(MaternalLine, FlowerHead) %>%
  summarise_all(.funs = "mean") %>%
  rename_with(.fn = function(x){str_remove(x, "current_30arcsec_") %>% str_remove("wc2.1_30s_")})%>%
  pivot_longer(cols = 3:37, names_to = "env_variables", values_to = "env_values")

p <- ggplot(data = Physaria_env_MaternalLine, aes(x = env_variables, y = env_values))+
  geom_violin(aes(fill = env_variables), alpha = 0.5)+
  geom_point(position = position_jitter(width = 0.05), size = 0.5)+
  geom_boxplot(fill = NA, outlier.shape = NA, width = 0.1, position = position_nudge(x = 0.5))+
  facet_wrap(.~env_variables, scales = "free", nrow = 6)+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_blank())

p
ggsave("./figures/Physaria_env_distribution.png", width = 12, height = 12)

# Create a matrix of coordinates
coords <- matrix(c(-91, 35, 
                   -91, 40, 
                   -84, 40, 
                   -84, 35, 
                   -91, 35), 
                 ncol = 2, byrow = TRUE)

# Create a polygon from the coordinates
polygon <- st_polygon(list(coords))

# Convert the polygon into an sf object
sf_polygon <- st_sfc(polygon, crs = 4326)

# Create a data frame to hold the sf object
sf_object <- st_sf(geometry = sf_polygon)


# Create PCA plot with scatter pies
set.seed(99)
p <- ggplot(data = Physaria_data_sf) +
  geom_sf(data = usa_state, fill = NA, color = "gray75") +
  geom_sf(data = canada_state, fill = NA, color = "gray75") +
  geom_sf(data = mexico_state, fill = NA, color = "gray75") +
  geom_sf()+
  geom_sf(data = sf_object, color = "red", linewidth = 1, fill = NA) + 
  geom_text_repel(data = Physaria_data_label_3857, aes(x = X, y = Y, label = MaternalLine), force_pull = 0.01, force = 30, max.overlaps = 25, size = 3, inherit.aes = FALSE,min.segment.length = 0.1)+
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "vertical", panel.grid = element_blank()) +
  coord_sf(xlim = c(-13e6, -8e6), ylim = c(2.5e6, 6e6), expand = FALSE, crs = 3857) +
  annotation_scale(location = "br", width_hint = 0.25)

ggsave("./figures/Physaria_map_20240709.png", width = 8, height = 6, dpi = 900)


# Extract legend from the PCA plot
p_legend <- get_legend(p + guides(color = "none", shape = "none") + theme(legend.background = element_blank()))


# Process environmental layer
env_layer <- raster("/Users/kuowenhsi/OneDrive - Washington University in St. Louis/Undergrad/Grace/Env_varibles/wc2.1_30s_bio_1.tif")

env_layer_croped <- crop(env_layer, extent(-91,-84,35,40))
env_layer_df <- as.data.frame(env_layer_croped, xy = TRUE) %>%
  drop_na()


# Create environmental layer plot
p_env <- ggplot(data = Physaria_data_sf) +
  geom_raster(data = env_layer_df, aes(x = x, y = y, fill = wc2.1_30s_bio_1)) +
  geom_sf(data = usa_state, fill = NA) +
  geom_sf(data = canada_state, fill = NA) +
  geom_sf(data = mexico_state, fill = NA) +
  geom_sf()+
  scale_fill_distiller(palette = "YlGnBu", name = "") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.key.height = unit(0.1, "in"), legend.key.width = unit(0.6, "in"), legend.margin = margin(t = -0.02, b = -0.02, unit = "in"), legend.position = "top", panel.background = element_rect(fill = "white")) +
  xlab(expression("Annual Mean Temp. (" * degree * C * ")")) +
  ylab("") +
  coord_sf(xlim = c(-91, -84), ylim = c(35, 40), expand = FALSE, label_axes = "")

ggsave("./figures/Physaria_AMT.png", width = 8, height = 6, dpi = 300)

# Combine PCA plot and environmental plot
p_comb <- p + guides(fill = "none") +
  annotation_custom(p_legend, xmin = -20e6, ymax = 3e6) +
  annotation_custom(ggplotGrob(p_env), xmin = -16e6, ymin = 2.75e6, xmax = -11e6, ymax = 4.75e6)

# Save the combined plot
ggsave("./figures/figure_1.png", width = 10, height = 8, dpi = 600)