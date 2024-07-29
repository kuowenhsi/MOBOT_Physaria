library(tidyverse)
library(RColorBrewer)
library(car)
library(agricolae)
library(ggpubr)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Physaria")

phy_tidy_data <- read_csv("./data/Physaria_MergedData_tidy_20240703.csv")

unique(phy_tidy_data$variable_name)
colnames(phy_tidy_data)
unique(phy_tidy_data$Longitude)
unique(phy_tidy_data$Latitude)

# Custom color palette with shades of pink and purple
palette <- c("thistle", "mediumpurple", "deeppink", "maroon3", "magenta", "pink", "lavender", "purple")


######

Physaria_leafArea <- filter(phy_tidy_data, variable_name %in% c("leafArea", "leafLong", "leafWide"), Date == as.Date("2024-06-11")) %>%
  group_by(Index, variable_name)%>%
  summarise(values = mean(values))%>%
  pivot_wider(names_from = variable_name, values_from = values)%>%
  mutate(leaf.long.wide = leafLong * leafWide)

p <- ggplot(data = Physaria_leafArea, aes(x = leafArea, y = leaf.long.wide))+
  geom_point()+
  stat_smooth(method = "lm")+
  stat_cor()+
  stat_regline_equation(label.y.npc = 0.8)+
  scale_y_continuous("Leaf length x width")+
  scale_x_continuous(expression("Detected green area ("~mm^2~")"))+
  theme_bw()
  
p

ggsave("./figures/Physaria_green_pixel_regression.png", width = 5, height = 3, dpi = 600)

#########



Physaria_leafArea <- filter(phy_tidy_data, variable_name == "leafArea") %>%
  mutate(MaternalLine = str_replace_all(MaternalLine, "-", "_")) %>%
  group_by(MaternalLine)%>%
  mutate(mean_leafArea = mean(values, na.rm = TRUE))


p <- ggplot(data = Physaria_leafArea, aes(x = reorder(MaternalLine, mean_leafArea), y = values, fill = MaternalLine)) +
  geom_point(position = position_jitter(width = 0.05), size = 0.5) +
  geom_boxplot(alpha = 0.5, outlier.fill = NA, outlier.shape = NA) +  # Using black for the outline to ensure visibility
  stat_anova_test(label.x.npc = 0.1)+
  scale_y_continuous(expression("Detected green area ("~mm^2~")"), expand = c(0.1, 2))+
  scale_x_discrete("")+
  scale_fill_manual(values = palette) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")
  
p

ggsave("./figures/Physaria_green.area_boxplot.png", width = 5, height = 3, dpi = 600)


#################
#########



Physaria_stemDia <- filter(phy_tidy_data, variable_name == "stemDia") %>%
  mutate(MaternalLine = str_replace_all(MaternalLine, "-", "_")) %>%
  group_by(MaternalLine)%>%
  mutate(mean_stemDia = mean(values, na.rm = TRUE))


p <- ggplot(data = Physaria_stemDia, aes(x = reorder(MaternalLine, mean_stemDia), y = values, fill = MaternalLine)) +
  geom_point(position = position_jitter(width = 0.05), size = 0.5) +
  geom_boxplot(alpha = 0.5, outlier.fill = NA, outlier.shape = NA) +  # Using black for the outline to ensure visibility
  stat_anova_test(label.x.npc = 0.1)+
  scale_y_continuous(expression("Stem Diameter ("~mm~")"))+
  scale_x_discrete("")+
  scale_fill_manual(values = palette) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

p

ggsave("./figures/Physaria_stemDia_boxplot.png", width = 5, height = 3, dpi = 600)


#################

Physaria_leafLong <- filter(phy_tidy_data, variable_name == "leafLong", Date == as.Date("2024-06-11")) %>%
  mutate(MaternalLine = str_replace_all(MaternalLine, "-", "_")) %>%
  group_by(MaternalLine)%>%
  mutate(mean_leafLong = mean(values, na.rm = TRUE))


p <- ggplot(data = Physaria_leafLong , aes(x = reorder(MaternalLine, mean_leafLong ), y = values, fill = MaternalLine)) +
  geom_point(position = position_jitter(width = 0.05), size = 0.5) +
  geom_boxplot(alpha = 0.5, outlier.fill = NA, outlier.shape = NA) +  # Using black for the outline to ensure visibility
  stat_anova_test(label.x.npc = 0.1)+
  scale_y_continuous(expression("leafLong ("~mm~")"))+
  scale_x_discrete("")+
  scale_fill_manual(values = palette) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

p

ggsave("./figures/Physaria_leafLong_boxplot.png", width = 5, height = 3, dpi = 600)


#################
#################

Physaria_leafWide <- filter(phy_tidy_data, variable_name == "leafWide", Date == as.Date("2024-06-11")) %>%
  mutate(MaternalLine = str_replace_all(MaternalLine, "-", "_")) %>%
  group_by(MaternalLine)%>%
  mutate(mean_leafWide = mean(values, na.rm = TRUE))


p <- ggplot(data = Physaria_leafWide , aes(x = reorder(MaternalLine, mean_leafWide ), y = values, fill = MaternalLine)) +
  geom_point(position = position_jitter(width = 0.05), size = 0.5) +
  geom_boxplot(alpha = 0.5, outlier.fill = NA, outlier.shape = NA) +  # Using black for the outline to ensure visibility
  stat_anova_test(label.x.npc = 0.1)+
  scale_y_continuous(expression("leafWide ("~mm~")"))+
  scale_x_discrete("")+
  scale_fill_manual(values = palette) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

p

ggsave("./figures/Physaria_leafWide_boxplot.png", width = 5, height = 3, dpi = 600)


#################



#########



Physaria_numRos <- filter(phy_tidy_data, variable_name == "numRos") %>%
  mutate(MaternalLine = str_replace_all(MaternalLine, "-", "_")) %>%
  group_by(MaternalLine)%>%
  mutate(mean_numRos = mean(values, na.rm = TRUE))


p <- ggplot(data = Physaria_numRos, aes(x = reorder(MaternalLine, mean_numRos), y = values, fill = MaternalLine)) +
  stat_summary(geom = "col", fun = "mean", alpha = 1)+
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))}, width = 0.2, aes(color = MaternalLine), alpha = 1)+
  stat_anova_test(label.x.npc = 0.1, label.y.npc = 0.35)+
  scale_y_continuous(expression("numRos"))+
  scale_x_discrete("")+
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

p

ggsave("./figures/Physaria_numRos_barplot.png", width = 5, height = 3, dpi = 600)


#################

#########



Physaria_numStem <- filter(phy_tidy_data, variable_name == "numStem") %>%
  mutate(MaternalLine = str_replace_all(MaternalLine, "-", "_")) %>%
  group_by(MaternalLine)%>%
  mutate(mean_numStem = mean(values, na.rm = TRUE))


p <- ggplot(data = Physaria_numStem, aes(x = reorder(MaternalLine, mean_numStem), y = values, fill = MaternalLine)) +
  stat_summary(geom = "col", fun = "mean", alpha = 1)+
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))}, width = 0.2, aes(color = MaternalLine), alpha = 1)+
  stat_anova_test(label.x.npc = 0.1, label.y.npc = 0.35)+
  scale_y_continuous(expression("numStem"))+
  scale_x_discrete("")+
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position = "none")

p

ggsave("./figures/Physaria_numStem_barplot.png", width = 5, height = 3, dpi = 600)


#################


leafArea <- aov(lm(values ~ MaternalLine, data = Physaria_leafArea))
leafArea
summary(leafArea)

HSD_result <- HSD.test(leafArea, "MaternalLine")

# Print the results of Tukey's HSD test
print(HSD_result)


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
