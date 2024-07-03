library(tidyverse)
library(dplyr)

setwd("C:/Users/maial/Downloads/MBG REU/MOBOT_Physaria")

list.files()
list.files(path = "./data")

phy_pheno <- read.csv("./data/Physaria_globosa_Phenotyping_Data_20240618.csv")  %>% 
  select(TemporaryID, MaternalLine, PlantingDate, FirstLeafDate, TransplantDate, everything()) %>% 
  separate(col = MaternalLine, into = c("MaternalLine", "FlowerHead"), sep = " ")

phy_lcms <- read.csv("./data/Physaria_LCMS_20240626.csv") %>% 
  mutate(MaternalLine = str_c(Accession, Planting, sep = "-")) %>% 
  select(-Accession, -Planting) %>% 
  select(MaternalLine, everything())

merged_data <- phy_pheno %>%
  left_join(phy_lcms, by = "MaternalLine") %>%
# filter(is.na(Latitude), is.na(Longitude)) %>%
  select(MaternalLine, FlowerHead, PlantingDate, FirstLeafDate, TransplantDate, Id, Country, State, County, Latitude, Longitude, Locality, Location.Details, everything())

colnames(merged_data)

write.csv(merged_data, "dataset_merge_maia_fix.csv")

