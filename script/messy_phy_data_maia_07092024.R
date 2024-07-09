
library(tidyverse)
library(RColorBrewer)

setwd("C:/Users/maial/Downloads/MBG REU/MOBOT_Physaria")

dataset_merge_maia_fix <- read_csv("data/dataset_merge_maia_fix.csv")
View(dataset_merge_maia_fix)

phy_data_fix <- dataset_merge_maia_fix %>%
  mutate(flwrBud.2 = case_when(flwrBud.2 == "Y" ~ 1,
                               flwrBud.2 == "N" ~ 0,
                               TRUE ~ as.numeric(NA))) %>% 
  mutate(flwrBud.1 = case_when(flwrBud.1 == "Y" ~ 1,
                               flwrBud.1 == "N" ~ 0,
                               TRUE ~ as.numeric(NA)))

phy_data_fix %>%
  select(starts_with("flwrBud")) %>%
  str()

phy_data <- as_tibble(phy_data_fix)
view(phy_data)

date_info <- phy_data %>%
  select(str_c("Date.", 1:15))%>% # add something
  .[1,]%>%
  pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
  mutate(Date_index = str_remove(Date_index, "Date."))%>%
  mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

phy_data_l <- phy_data %>%
  # Select the columns needed
  select(TemporaryID, RanIndex, 
         starts_with("leafLong"), starts_with("leafWide"), 
         starts_with("numRos"), starts_with("numStem"), starts_with("stemDia"), 
         starts_with("stemLength"), starts_with("numLeaf"), starts_with("flwrBud"), 
         starts_with("flwrOpen"), starts_with("fruitPres"), starts_with("fruitDeh"), 
         starts_with("numFlowerStem")) %>% 
  
  # Convert selected columns to double
  mutate(across(contains(c("leafLong", "leafWide", "numRos", "numStem", 
                           "stemDia", "stemLength", "numLeaf", "flwrBud", "flwrOpen", 
                           "fruitPres", "fruitDeh", "numFlowerStem")), as.double)) %>% 
  
  # Pivot the data from wide to long format
  pivot_longer(cols = c(starts_with("leafLong"), starts_with("leafWide"), 
                        starts_with("Surv"), starts_with("numRos"), starts_with("numStem"), 
                        starts_with("stemDia"), starts_with("stemLength"), 
                        starts_with("numLeaf"), starts_with("flwrBud"), 
                        starts_with("flwrOpen"), starts_with("fruitPres"), 
                        starts_with("fruitDeh"), starts_with("numFlowerStem")), 
               names_to = c("variable_name", "Date_index"), 
               names_sep = "[.]",
               values_to = "values") %>%
  
  # Join with date_info to get date
  left_join(date_info, by = "Date_index") %>%
  
  # Convert the date column to Date format
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  
  # Group by the necessary variables
  group_by(TemporaryID, RanIndex, variable_name)

# Print the result
print(phy_data_l)

print(phy_data_l$values)

phy_green_area <- read_tsv("data/Physaria_green_area_20240611.txt")

######################################################################

ggplot(data = dataset_merge_maia_fix, aes(x = MaternalLine, y = leafLong.10)) +
  geom_point(color = "deeppink") +
  labs(x = "Maternal Line", y = "Leaf Length (leafLong.10)") +
  theme_bw()

ggplot(data = dataset_merge_maia_fix, aes(x = MaternalLine, y = leafLong.10, fill = MaternalLine)) +
  geom_violin(color = "deeppink") +
  labs(x = "Maternal Line", y = "Leaf Length (leafLong.10)") +
  theme_bw()

phy_tidy_data <- read_csv("data/Physaria_MergedData_20240703.csv")

# Ensure the columns are numeric
phy_tidy_data <- phy_tidy_data %>%
  mutate(
    leafArea.10 = as.numeric(as.character(leafArea.10)),
    leafLong.10 = as.numeric(as.character(leafLong.10)),
    leafWide.10 = as.numeric(as.character(leafWide.10))
  )

# Filter out rows with NA values in the specified columns
filtered_data <- phy_tidy_data %>%
  filter(!is.na(leafArea.10) & !is.na(leafLong.10))

# Custom color palette with shades of pink and purple
palette <- c("thistle", "mediumpurple", "deeppink", "maroon3", "magenta", "pink", "lavender", "purple")

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
ggplot(data = filtered_data, aes(x = MaternalLine, y = leafArea.10, fill = MaternalLine)) +
  geom_violin(color = "black") +  # Using black for the outline to ensure visibility
  scale_fill_manual(values = palette) +
  theme_bw()

ggsave("./figures/phy_maternal_area_violin_maia.png", width = 10, height = 8, dpi = 600)

ggplot(data = filtered_data, aes(x = MaternalLine, y = leafArea.10)) +
  geom_point(color = "deeppink") +
  labs(x = "Maternal Line", y = "Leaf Area at Week 10 (leafArea.10)") +
  theme_bw()

ggsave("./figures/phy_maternal_area_point_maia.png", width = 10, height = 8, dpi = 600)

############################################################################################

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
ggplot(data = filtered_data, aes(x = MaternalLine, y = leafLong.10, fill = MaternalLine)) +
  geom_violin(color = "black") +  # Using black for the outline to ensure visibility
  scale_fill_manual(values = palette) +
  labs(x = "Maternal Line", y = "Leaf Length (leafLong.10)") +
  theme_bw()

ggsave("./figures/phy_maternal_length_maia_07082024.png", width = 10, height = 8, dpi = 600)

#################################################################################################

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

######################################################################################

# Plotting using Facet Wrap
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafWide.10, color = MaternalLine)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +  # Add linear regression line
  labs(x = "Green Pixel Area", y = "Leaf Width at Week 10 (leafWide.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafWide.10)), ceiling(max(filtered_data$leafWide.10)), by = 1)) +
  facet_wrap(~ MaternalLine)  # Replace 'maternal_line' with the actual column name for maternal lines

ggsave("./figures/phy_facet_maternal_07092024.png", width = 10, height = 8, dpi = 600)

######################################################################################################################################

# Transform "flwrOpen.2" to numeric

filter_data_fix <- filtered_data %>%
  mutate(flwrBud.2 = case_when(flwrBud.2 == "Y" ~ 1,
                               flwrBud.2 == "N" ~ 0,
                               TRUE ~ as.numeric(NA)))