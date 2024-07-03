
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

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

phy_data_fix%>%
  select(starts_with("flwrBud"))%>%
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

ggplot(data = dataset_merge_maia_fix, aes(x = MaternalLine, y = leafLong.10)) +
  geom_point() +
  labs(x = "Leaf Length (leafLong.10)", y = "Maternal Line") +
  theme_bw()

ggsave("./figures/phy_maternal_maia.png", width = 10, height = 8, dpi = 600)

phy_green_area_fix <- phy_green_area %>% 
  mutate(id = as.numeric(id))

merged_data <- phy_data_l %>%
  left_join(phy_green_area_fix, by = c("id" = "TemporaryID"))%>%
  left_join(phy_data_l, by = "MaternalLine")
