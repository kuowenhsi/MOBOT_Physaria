install.packages("ggplot2")
install.packages("maps")
install.packages("sf") # for handling spatial data
library(ggplot2)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

usa_map <- map_data("state")

library(readr)
dataset_merge_maia_fix <- read_csv("data/dataset_merge_maia_fix.csv")
View(dataset_merge_maia_fix)

usa_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()
canada_state <- ne_states(country = "canada", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()
mexico_state <- ne_states(country = "mexico", returnclass = "sf") %>%
  st_crop(xmin = -135, ymin = -55, xmax = -65, ymax = 60) %>%
  st_geometry()

p <- ggplot(data = st_as_sf(dataset_merge_maia_fix, coords = c("Longitude", "Latitude"), agr = "constant", crs = 4326)) +
  geom_sf(data = usa_state, fill = "thistle1", color = "darkgreen") +
  geom_sf(data = canada_state, fill = "lightgreen", color = "hotpink") +
  geom_sf(data = mexico_state, fill = NA, color = "purple3") +
  geom_sf(size = 4, aes(color = MaternalLine, shape = MaternalLine)) +
  scale_shape_manual(values = 1:8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "vertical", panel.grid = element_blank()) +
  coord_sf(ylim = c(4e6, 5e6), xlim = c(-10.5e6,-9e6), expand = FALSE, crs = 3857)

# Print the plot
print(p)

ggsave("./figures/phy_map_maia_county.png", width = 10, height = 8, dpi = 600)

#########################################

ggplot(data = usa_map) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "deeppink", color = "black") +
  theme_light() +
  coord_fixed(1.3) +
  labs(title = "Map of the USA")

# Create a data frame with state centers for labeling
state_centers <- data.frame(state.center, state.abb)

ggplot(data = usa_map) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "deeppink", color = "black") +
  geom_text(data = state_centers, aes(x = x, y = y, label = state.abb), size = 3, color = "white") +
  theme_light() +
  coord_fixed(1.3) +
  labs(title = "Map of the USA with State Names")

# Install and load necessary packages
install.packages("ggplot2")
install.packages("maps")
library(ggplot2)
library(maps)

# Load the map data
usa_map <- map_data("state")

# Install and load necessary packages
install.packages("ggplot2")
install.packages("maps")
library(ggplot2)
library(maps)

# Load the map data
usa_map <- map_data("state")

# Example data frame with states and values
state_data <- data.frame(
  region = tolower(state.name),
  value = runif(50, min = 0, max = 100)  # Random values for illustration
)

# Merge map data with the example data
usa_map_data <- merge(usa_map, state_data, by = "region")

# Define a pink color palette for gradient
pink_palette <- c("#FFC0CB", "#FF69B4", "#FF1493", "#DB7093", "#C71585")

# Plot the map with the pink gradient effect
ggplot(data = usa_map_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = value, color = value), 
               size = 0.2, alpha = 0.7) +
  scale_fill_gradient(low = pink_palette[1], high = pink_palette[length(pink_palette)], 
                      limits = range(usa_map_data$value),
                      (range(usa_map_data$value))) +
  scale_color_gradient(low = "white", high = "black") +
  theme_minimal() +
  coord_fixed(1.3) +
  labs(title = "Map of the USA with Pink Shiny Gradient States", fill = "Value", color = "Value")

ggplot()

