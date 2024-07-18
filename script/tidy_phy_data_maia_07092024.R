
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

setwd("C:/Users/maial/Downloads/MBG REU/MOBOT_Physaria")

phy_tidy_data <- read_csv("data/physaria_buf_climate_data_20240717_l.csv")
unique(phy_tidy_data$variable_name)

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
p <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = wc2.1_30s_bio_1, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Mean Temperature")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p  

ggsave("./figures/phy_figure_bio1.png", width = 10, height = 8, dpi = 600)

p2 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = wc2.1_30s_bio_7, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Temperature Annual Range")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p2  

p3 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = wc2.1_30s_bio_12, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Precipatation")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p3  

p4 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = wc2.1_30s_elev, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Elevation")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p4

p5 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = wc2.1_30s_elev, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Elevation")+
  scale_y_continuous(name = "Leaf Length (cm)")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p5

p6 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = wc2.1_30s_bio_1, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Mean Temperature")+
  scale_y_continuous(name = "Leaf Length (cm)")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p6

p7 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = wc2.1_30s_bio_12, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Precipatation")+
  scale_y_continuous(name = "Leaf Long")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p7

p8 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = wc2.1_30s_bio_7, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "red")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Temperature Annual Range")+
  scale_y_continuous(name = "Leaf Long")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

p8  

############################################################################################

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
p2 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  scale_fill_manual(values = palette) +
  labs(x = "Maternal Line", y = "Leaf Length (cm)") +
  theme_bw() +
  theme(legend.position = "none")

p2

ggsave("./figures/phy_maternal_length_2_maia_07082024.png", width = 10, height = 8, dpi = 600)

p4 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = Latitude, y = values, fill = Latitude)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Maternal Line", y = "Leaf Length (cm)") +
  theme_bw() +
  theme(legend.position = "none")

p4

#################################################################################################

# Plotting Phenotype Traits in Relation to Green Pixel Area

# Leaf Length at Week 10 by Green Pixel Area

scatterplot_data <- filter(phy_tidy_data, variable_name %in% c("leafLong", "leafArea"))%>%
  filter(Date == as.Date("2024-06-11"))%>%
  pivot_wider(names_from = variable_name, values_from = values)%>%
  rowwise()%>%
  mutate(leafLong = mean(leafLong), leafArea = mean(leafArea))%>%
  ungroup()

# Create the plot
p3 <- ggplot(data = scatterplot_data, aes(x = Latitude, y = leafLong)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Latitude", y = "Leaf Length") +
  theme_bw() +
  theme(legend.position = "none")

p3

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

######################################################################################

# Plotting using Facet Wrap
ggplot(data = scatterplot_data, aes(x = leafArea, y = leafLong, color = MaternalLine)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +  # Add linear regression line
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95) +
  labs(x = "Green Pixel Area", y = "Leaf Width at Week 10 (leafWide.10)") +
  theme_bw() +
  facet_wrap(~ MaternalLine)  # Replace 'maternal_line' with the actual column name for maternal lines

ggsave("./figures/phy_facet_maternal_07092024.png", width = 10, height = 8, dpi = 600)

######################################################################################################################################

# Transform "flwrOpen.2" to numeric

filter_data_fix <- filtered_data %>%
  mutate(flwrBud.2 = case_when(flwrBud.2 == "Y" ~ 1,
                               flwrBud.2 == "N" ~ 0,
                               TRUE ~ as.numeric(NA)))

unique(phy_tidy_data$Longitude)
unique(phy_tidy_data$FlowerHead)

################################################################################################################################

# Geographic Analysis

################################################################################################################################################

library(ggplot2)
library(dplyr)

# Calculate the mean leaf area for each MaternalLine
means <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  group_by(MaternalLine) %>%
  summarize(mean_value = mean(values))

# Reorder the MaternalLine factor based on the calculated means
phy_tidy_data <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  mutate(MaternalLine = factor(MaternalLine, levels = means$MaternalLine[order(means$mean_value)]))

# Create the violin plot
p1 <- ggplot(data = phy_tidy_data, aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) + # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")")) +
  labs(x = "Maternal Line", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

p1

library(ggplot2)
library(dplyr)

# Calculate the mean leaf area for each MaternalLine
means <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  group_by(MaternalLine) %>%
  summarize(mean_value = mean(values))

# Reorder the MaternalLine factor based on the calculated means
phy_tidy_data <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  mutate(MaternalLine = factor(MaternalLine, levels = means$MaternalLine[order(means$mean_value)]))

# Create the box plot
p1 <- ggplot(data = phy_tidy_data, aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_boxplot(color = "black", alpha = 0.4) + # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")")) +
  labs(x = "Maternal Line", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

p1

library(ggplot2)
library(dplyr)

# Calculate the mean leaf area for each MaternalLine
means <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  group_by(MaternalLine) %>%
  summarize(mean_value = mean(values))

# Reorder the MaternalLine factor based on the calculated means
ordered_levels <- means %>%
  arrange(mean_value) %>%
  pull(MaternalLine)

phy_tidy_data <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  mutate(MaternalLine = factor(MaternalLine, levels = ordered_levels))

# Create the box plot
p1 <- ggplot(data = phy_tidy_data, aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_boxplot(color = "black", alpha = 0.4) + # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")")) +
  labs(x = "Maternal Line", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

p1

# Make graph with accession as x and traits as y

# Find climate variables with significance (rainfall, elevation, temperature, solar radiation)

#####################################################################################################

# ANOVA

install.packages("esquisse")
library(esquisse)
esquisser(phy_data)

phy_data <- read_csv("data/physaria_buf_climate_data_20240717.csv")

# list of traits: Leaf Area, Leaf Length, Leaf Width

ggplot(data = phy_data, aes(x = wc2.1_30s_bio_16, y = leafLong.10)) +
  geom_point(color = "deeppink") +
  theme_bw() 
