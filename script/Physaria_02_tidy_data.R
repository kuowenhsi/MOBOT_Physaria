library(tidyverse)
library(writexl)

setwd("C:/Users/maial/Downloads/MBG REU/MOBOT_Physaria")


choose_big_number <- function(x){
  x <- as.character(x)
  x_splited <- str_split(str_remove_all(x, " "), "/")
  x_big <- lapply(x_splited, function(x){x = as.numeric(x); x = sort(x, decreasing = TRUE, na.last = TRUE); return(x[[1]])})
  return(unlist(x_big))
}

convert_Y_one <- function(x) {
  case_when(
    x == "Y" ~ 1,
    x == "N" ~ 0,
    TRUE ~ NA_real_
  )
}

physaria_buf_climate <- read_csv("./data/physaria_buf_climate_data_20240717.csv", na = c("", "NA", "N/A", "DNR"))%>%
  mutate(stemDia.1 = choose_big_number(stemDia.1), stemLength.1 = choose_big_number(stemLength.1))%>%
  mutate_at(.vars = all_of(c("flwrBud.1", "flwrBud.2", "flwrOpen.1", "flwrOpen.2", "fruitPres.1", "fruitPres.2", "fruitPres.3", "fruitPres.4", "fruitDeh.1", "fruitDeh.2", "fruitDeh.3", "fruitDeh.4")), .funs = "convert_Y_one")
  

colnames(physaria_buf_climate)
str(physaria_buf_climate)

## check data class

# print unique column names

uni_colnames <- unique(str_split_i(colnames(physaria_buf_climate), "[.]", 1))[16:30]
uni_colnames

sapply(uni_colnames, function(x){select(physaria_buf_climate, matches(x)) %>% str()})

date_info <- physaria_buf_climate %>%
  select(str_c("Date.", 1:15))%>% # add something
  .[1,]%>%
  pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
  mutate(Date_index = str_remove(Date_index, "Date."))%>%
  mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

physaria_buf_climate_l <- physaria_buf_climate %>%
  # Select the columns needed
  select(Index, TemporaryID, MaternalLine, FlowerHead, State, County, Longitude, Latitude, wc2.1_30s_bio_1, wc2.1_30s_bio_2, wc2.1_30s_bio_3, wc2.1_30s_bio_4, wc2.1_30s_bio_5, wc2.1_30s_bio_6, wc2.1_30s_bio_7, wc2.1_30s_bio_8, wc2.1_30s_bio_9, wc2.1_30s_bio_10, wc2.1_30s_bio_11, wc2.1_30s_bio_12, wc2.1_30s_bio_13, wc2.1_30s_bio_14, wc2.1_30s_bio_15, wc2.1_30s_bio_16, wc2.1_30s_bio_17, wc2.1_30s_bio_18, wc2.1_30s_bio_19, wc2.1_30s_elev, starts_with(uni_colnames[3:15])) %>% 
  # Pivot the data from wide to long format
  pivot_longer(cols = starts_with(uni_colnames[3:15]), 
               names_to = c("variable_name", "Date_index"), 
               names_sep = "[.]",
               values_to = "values") %>%
  # Join with date_info to get date
  left_join(date_info, by = "Date_index") %>%
  # Convert the date column to Date format
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) 

unique(physaria_buf_climate_l$variable_name)

write_csv(physaria_buf_climate_l, "./data/physaria_buf_climate_data_20240717_l.csv")
write_xlsx(physaria_buf_climate_l, "./data/physaria_buf_climate_data_20240717_l.xlsx")
