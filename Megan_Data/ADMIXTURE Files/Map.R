

####################################################################################
############################ Admixture Map by County ###############################
####################################################################################

library(tidyverse)
library(sf)
library(rnaturalearth)
library(tigris)
library(scatterpie)
library(ggspatial)
library(cowplot)

setwd("~/Desktop/Physaria_Data/VCF-Plink-Remove-Admixture/Admixture_files")
#!!! change line 28 to corresponding K value .Q file
#!!! change line 144 to corresponding # of ancestry lines

#### 1) Load metadata and clean sample names ####
metadata <- read_csv("Physaria_MergedData_20240703.csv") %>%
  dplyr::rename(Sample_Name = !!sym("Index")) %>%
  mutate(
    Sample_Name = as.character(Sample_Name),
    Sample_Name = paste0("Physaria_", str_pad(Sample_Name, width = 3, pad = "0"))
  )

#### 2) Load in Q filed for K = # as qmat ####
qmat <- read_table("Physaria_Chr_1_ldpruned.5.Q", col_names = FALSE) %>%
  mutate(Sample_Name = read_table("Physaria_Chr_1_ldpruned.fam", col_names = FALSE) %>% pull(X2))%>%
  left_join(metadata %>% select(1:14, "Sample_Name"), by = "Sample_Name")%>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Ancestry", 
    values_to = "Proportion"
  )

#### 3) Summarize mean proportions ####
qmat_summary <- qmat |> 
  group_by(MaternalLine, Ancestry) |> 
  summarise(
    mean_proportion = mean(Proportion),
    Latitude = dplyr::first(Latitude),
    Longitude = dplyr::first(Longitude),
    County = dplyr::first(County),
    .groups = "drop"
  )

#### 4) Convert to wide format (for pie charts) ####
df_wide <- qmat_summary %>%
  pivot_wider(names_from = Ancestry, values_from = mean_proportion) %>%
  distinct(MaternalLine, .keep_all = TRUE)

#### 5) Manual label shifting ####
label_shifts <- tribble(
  ~MaternalLine,     ~County,      ~adj_X_shift, ~adj_Y_shift,
  "2021-1841-1",     "Franklin1",       -30000,        -100000,
  "2021-1843-2",     "Franklin2",       -90000,        -60000,
  "2023-0185-2",     "Posey",         40000,       -50000,
  "2021-0838-1",     "Montgomery",    -50000,        60000,
  "2021-1836-2",     "Cheatham1",      -20000,       120000,
  "2021-1839-2",     "Cheatham2",       30000,        150000,
  "2021-1838-2",     "Davidson",       50000,        90000,
  "2018-1133-2",     "Trousdale",     40000,       90000
)

#### 6) Convert to sf, transform CRS, coordinates ####
df_sf <- st_as_sf(df_wide, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3857) %>%
  mutate(
    X = st_coordinates(.)[,1],
    Y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

#### 7) Join label shifts ####
df_pie <- df_sf |> 
  left_join(label_shifts, by = "MaternalLine") |> 
  mutate(
    adj_X = X + adj_X_shift,
    adj_Y = Y + adj_Y_shift
  ) |> 
  select(-adj_X_shift, -adj_Y_shift)

#### 8) Rename MaternalLines for Labels ####
maternal_line_labels <- c(
  "2021-1843-2" = "Franklin2",
  "2023-0185-2" = "Posey",
  "2021-1838-2" = "Davidson",
  "2021-1836-2" = "Cheatham1",
  "2021-0838-1" = "Montgomery",
  "2018-1133-2" = "Trousdale",
  "2021-1839-2" = "Cheatham2",
  "2021-1841-1" = "Franklin1"
)
df_pie <- df_pie %>%
  mutate(MaternalLine = maternal_line_labels[MaternalLine])


#### 9) Get USA states ####
usa_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii")) %>%
  st_transform(3857)

kentucky <- filter(usa_states, name == "Kentucky")
tennessee <- filter(usa_states, name == "Tennessee")
indiana <- filter(usa_states, name == "Indiana")

state_names <- c("tennessee", "kentucky", "indiana")
subset_map <- map_data("state") %>% subset(region %in% state_names)
county_subset <- map_data("county") %>% subset(region %in% state_names)

options(tigris_class = "sf")
all_counties <- bind_rows(
  counties(state = "TN", cb = TRUE),
  counties(state = "KY", cb = TRUE),
  counties(state = "IN", cb = TRUE),
) %>% st_transform(3857)

illinois <- bind_rows(
  counties(state = "IL", cb = TRUE)
) %>% st_transform(3857)

#### 10) Load in and crop rivers ####
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
rivers_cropped <- st_crop(rivers, xmin = -135, ymin = -55, xmax = -65, ymax = 60)

#### 11) Map ####
main_map <- ggplot() +
  geom_sf(data = kentucky, fill = NA, color = "black") +
  geom_sf(data = all_counties, fill = "lightgray", color = "gray65", size = 0.2) +
  geom_sf(data = illinois, fill = "lightgray", color = NA, size = 0.2) +
  # Halo/background river (light color, slightly thicker)
  geom_sf(data = rivers_cropped,
          color = "white",
          size = 1.8,
          inherit.aes = FALSE) +
  # Rivers
  geom_sf(data = rivers_cropped,
          color = "dodgerblue3",
          size = 5,
          inherit.aes = FALSE) +
  # Point to Pie Chart
  geom_segment(data = df_pie,
               aes(x = X, y = Y, xend = adj_X, yend = adj_Y),
               linetype = "solid", color = "gray50") +
  # Pies
  geom_scatterpie(data = df_pie,
                  aes(x = adj_X, y = adj_Y, r = 20000),
                  cols = c("X1", "X2", "X3", "X4", "X5"),
                  color = NA) +
  # Maternal Line Label
  geom_text(data = df_pie,
            aes(x = adj_X, y = adj_Y, label = MaternalLine),
            size = 3, nudge_y = 25000, fontface = "bold") +
  # Show pie chart point
  geom_point(data = df_pie,
             aes(x = X, y = Y),
             size = 1,
             color = "black") +
  coord_sf(xlim = c(-9.9e6, -9.4e6), ylim = c(4.3e6, 4.7e6), expand = FALSE, crs = 3857) +
  annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.6) +
  scale_fill_manual(
    name = "Ancestry",
    values = c( "X1" = "#d95f02",
                "X2" = "#1b9e77", 
                "X3" = "#7570b3", 
                "X4" = "#e6ab02", 
                "X5" = "#e7298a",
                "X6" = "#66a61e", 
                "X7" = "#a6cee3",
                "X8" = "#666666",
                "X9" = "#1f78b4",
                "X10" = "#BC80BD")) +
  labs(title = NULL) +  
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    bar_cols = c("black", "white"),
    unit = "km"
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering
  )

#### 12) Show and save map ####
main_map
ggsave("Physaria_ancestry_map_mainK7.png", plot = main_map,
       width = 6, height = 5, dpi = 300, units = "in", bg = "white")

#### 13) Prepare minimap showing locations in the USA map ####
county_points <- st_as_sf(df_wide, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(kentucky))
inset_map <- ggplot() +
  geom_sf(data = usa_states, fill = "white", color = "black") +
  geom_sf(data = kentucky, fill = "darkgray", alpha = 0.8, color = "black") +
  geom_sf(data = tennessee, fill = "darkgray", alpha = 0.8, color = "black") +
  geom_sf(data = indiana, fill = "darkgray", alpha = 0.8, color = "black") +
  geom_sf(data = county_points, color = "red", size = 0.5) +
  theme_void() 

#### 14) Create an inset panel with white background and border ####
inset_with_box <- ggdraw() +
  # White background
  draw_plot(
    ggplot() + theme_void() + 
      theme(panel.background = element_rect(fill = "white")),
    x = 0, y = .17, width = 1, height = .67
  ) +
  # Map
  draw_plot(inset_map, x = 0, y = 0, width = 1, height = 1)

#### 15) Combine with the Main Map ####
full_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_with_box, x = 0.02, y = 0.632, width = 0.4, height = 0.4)

#### 16) Print and Save ####
print(full_map)
ggsave("Physaria_ancestry_map_fullK5.png", plot = full_map,
       width = 6, height = 5, dpi = 300, units = "in", bg = "white")
