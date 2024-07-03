library(tidyverse)
library(writexl)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Physaria")

list.files(path = "./data")

Physaria_pheno <- read.csv("./data/Physaria_globosa_Phenotyping_Data_20240618.csv")  %>% 
  select(TemporaryID, MaternalLine, PlantingDate, FirstLeafDate, TransplantDate, everything()) %>% 
  separate(col = MaternalLine, into = c("MaternalLine", "FlowerHead"), sep = " ")%>%
  rename(Index = RanIndex)

Physaria_LCMS <- read.csv("./data/Physaria_LCMS_20240626.csv") %>%
  mutate(MaternalLine = str_c(Accession, Planting, sep = "-")) %>%
  select(MaternalLine, everything())

Physaria_GreenArea <- read_tsv("data/Physaria_green_area_20240611.txt") %>%
  mutate(Index = as.numeric(id)) %>%
  select(Index, leafArea.10 = leaf_area)


Physaria_MergedData <- Physaria_pheno %>%
  left_join(Physaria_GreenArea, by = "Index") %>%
  left_join(Physaria_LCMS, by = "MaternalLine") %>%
  select(Index, MaternalLine, FlowerHead, PlantingDate, FirstLeafDate, State, County, Latitude, Longitude, everything())%>%
  select(where(~ !all(is.na(.))), -Collection.Num)

colnames(Physaria_MergedData)

write_csv(Physaria_MergedData, "./data/Physaria_MergedData_20240703.csv")
write_xlsx(Physaria_MergedData, "./data/Physaria_MergedData_20240703.xlsx")
