library(tidyverse)
library(RColorBrewer)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Physaria")

phy_tidy_data <- read_csv("data/Physaria_MergedData_tidy_20240703.csv")
unique(phy_tidy_data$variable_name)

# Custom color palette with shades of pink and purple
palette <- c("thistle", "mediumpurple", "deeppink", "maroon3", "magenta", "pink", "lavender", "purple")

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
p <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black") +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  scale_fill_manual(values = palette) +
  theme_bw()

ggsave("./figures/phy_maternal_area_violin_maia.png", width = 10, height = 8, dpi = 600)

p <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")"))+
  theme_bw()+
  theme(legend.position = "none")

ggsave("./figures/phy_maternal_area_point_maia.png", width = 10, height = 8, dpi = 600)

################################################################################

# Custom color palette with shades of pink and purple
palette <- c("thistle", "mediumpurple", "deeppink", "maroon3", "magenta", "pink", "lavender", "purple")

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
ggplot(data = filtered_data, aes(x = MaternalLine, y = leafLong.10, fill = MaternalLine)) +
  geom_violin(color = "black") +  # Using black for the outline to ensure visibility
  scale_fill_manual(values = palette) +
  labs(x = "Maternal Line", y = "Leaf Length (leafLong.10)") +
  theme_bw()

ggsave("./figures/phy_maternal_length_maia_07082024.png", width = 10, height = 8, dpi = 600)

##################################################################################

# Plotting Phenotype Traits in Relation to Green Pixel Area

# Leaf Length at Week 10 by Green Pixel Area

# Create the plot
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafLong.10)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Green Pixel Area", y = "Leaf Length at Week 10 (leafLong.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafLong.10)), ceiling(max(filtered_data$leafLong.10)), by = 1))

ggsave("./figures/phy_length_area_07082024.png", width = 10, height = 8, dpi = 600)

# Create the plot and color by the variable "MaternalLine"
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafLong.10, color = MaternalLine)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +
  labs(x = "Green Pixel Area", y = "Leaf Length at Week 10 (leafLong.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafLong.10)), ceiling(max(filtered_data$leafLong.10)), by = 1))

ggsave("./figures/phy_length_area_maternal_07082024.png", width = 10, height = 8, dpi = 600)

# Leaf Width at Week 10 by Green Pixel Area

# Plot
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafWide.10)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Green Pixel Area", y = "Leaf Width at Week 10 (leafWide.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafWide.10)), ceiling(max(filtered_data$leafWide.10)), by = 1))

ggsave("./figures/phy_wide_area_07082024.png", width = 10, height = 8, dpi = 600)

# Plot and color by the variable "MaternalLine"
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafWide.10, color = MaternalLine)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +
  labs(x = "Green Pixel Area", y = "Leaf Wide at Week 10 (leafWide.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafWide.10)), ceiling(max(filtered_data$leafWide.10)), by = 1))

ggsave("./figures/phy_wide_area_maternal_07082024.png", width = 10, height = 8, dpi = 600)

# Flowering by Green Pixel Area

# Plot
ggplot(data = filtered_data, aes(x = leafArea.10, y = flwrOpen.15)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Green Pixel Area", y = "Flower Open at Week 15 (flwrOpen.15)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$flwrOpen.15)), ceiling(max(filtered_data$flwrOpen.15)), by = 1))

ggsave("./figures/phy_flwr_area_07082024.png", width = 10, height = 8, dpi = 600)
