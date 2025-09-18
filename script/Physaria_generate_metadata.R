library(tidyverse)   

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Physaria")

Physaria_data <- readr::read_csv("./data/Physaria_MergedData_20240703.csv")%>%
  select(c(1:15, 113:140))%>%
  rename(Sample_Species = Species)%>%
  mutate(Sample_Name = paste("Physaria", str_pad(Index, 3 ,pad = "0"), sep = "_"))%>%
  select(Sample_Name, Sample_Name, MaternalLine, FlowerHead, State, County, Latitude, Longitude, everything())

colnames(Physaria_data)

Physaria_library <-readxl::read_xlsx("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/DNA_stock/DNA_stock_Physaria150_dilution.xlsx")%>%
  select(Sample_Name)%>%
  bind_rows(data.frame(Sample_Name = "Physaria_267"))%>%
  left_join(Physaria_data, by = "Sample_Name")%>%
  mutate(FlowerHead = as.character(FlowerHead))

Physaria_env <- read_csv("./data/Physaria_buf_climate_data_20240724.csv")

Physaria_other_collections <- read_csv("./data/Physaria_other_collections.csv")%>%
  rename(FlowerHead = `Voucher number/ code`)%>%
  separate(`State, County, Locality (Element Occurrence number)`, into = c("State", "County", "Locality"), sep = ", ")%>%
  mutate(State = case_when(State == "KY" ~ "Kentucky", State == "TN" ~ "Tennessee"))

# writexl::write_xlsx(Physaria_other_collections, "./data/Physaria_other_collections_20250918.xlsx")

Physaria_other_collections <- readxl::read_xlsx("./data/Physaria_other_collections_20250918.xlsx")

colnames(Physaria_other_collections)

Physaria_DNA_stock <- readxl::read_xlsx("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/DNA_stock/DNA_stock_Physaria.xlsx")%>%
  filter(Library == "T")%>%
  left_join(Physaria_other_collections, by = "FlowerHead")%>%
  select(Sample_Name, Sample_Name, MaternalLine = FlowerHead, FlowerHead = MaternalLine, State, County, Latitude = Lat, Longitude = Long, Index = index, everything())%>%
  mutate(MaternalLine = as.character(MaternalLine))
  
Physaria_metadata <- bind_rows(Physaria_library, Physaria_DNA_stock)

writexl::write_xlsx(list(Physaria_metadata=Physaria_metadata), path = "./data/Physaria_metadata_20250918.xlsx")

Physaria_metadata_County <- Physaria_metadata %>%
  select(Sample_Name, County, State)%>%
  mutate(County = paste(County, "(", State, ")", sep = "_"))%>%
  select(Sample_Name, County)

write_csv(Physaria_metadata_County, file = "./data/Physaria_metadata_County.csv",col_names = FALSE)
