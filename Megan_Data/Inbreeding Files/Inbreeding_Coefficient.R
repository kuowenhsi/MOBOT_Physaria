

####################################################################################
################## Inbreeding Coefficient by MaternalLine ##########################
####################################################################################

library(tidyverse)
library(dplyr)
library(gridExtra)

setwd("~/Desktop/Physaria_Data/Inbreeding_Data")

#### 1) Load in Inbreeding data ####
het_data <- read.table("Physaria_Chr_1_1-output.het", header = TRUE)

#### 2) Load in samples that need to be removed ####
remove_list <- read.table("remove_samplescopy.txt", header = FALSE, sep = "\t") %>% pull(1)

#### 3) Remove samples ####
filtered_het <- het_data %>%
  filter(!INDV %in% remove_list)

#### 4) Load in Meta Data ####
metadata <- read_csv("Physaria_MergedData_20240703.csv") %>%
  mutate(Index = paste0("Physaria_", str_pad(as.character(Index), width = 3, pad = "0")))

#### 5) Rename MaternalLines ####
line_map <- tibble::tibble(
  MaternalLine = c(
    "2021-1843-2",
    "2023-0185-2",
    "2021-1838-2",
    "2021-1836-2",
    "2021-0838-1",
    "2018-1133-2",
    "2021-1839-2",
    "2021-1841-1"
  ),
  POP = c("Franklin2", "Posey", "Davidson", "Cheatham1", "Montgomery", "Trousdale", "Cheatham2", "Franklin1")
)

#### 6) Join metadata and rename MaternalLines ####
metadata_named <- metadata %>%
  left_join(line_map, by = "MaternalLine") %>%
  mutate(MaternalLineRenamed = if_else(!is.na(POP), POP, MaternalLine))

#### 7) Join with het data ####
het_data_joined <- filtered_het %>%
  left_join(metadata_named %>% select(Index, MaternalLineRenamed), 
            by = c("INDV" = "Index"))

#### 8) Set custom plotting order ####
ordered_levels <- c("Franklin2", "Franklin1", "Posey", "Montgomery", "Cheatham1", "Cheatham2", "Davidson", "Trousdale")
het_data_joined$MaternalLineRenamed <- factor(het_data_joined$MaternalLineRenamed, levels = ordered_levels)

####################################################################################
############################### Simple Histogram ###################################
####################################################################################

p <- ggplot(het_data_joined, aes(x = F)) +
  geom_histogram(binwidth = 0.02, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Inbreeding Coefficient (F)",
       x = "Inbreeding Coefficient (F)",
       y = "Count") +
  theme_bw()

p

ggsave("F_distribution_histogram.png", plot = p, width = 7, height = 5, dpi = 300)

####################################################################################
########################## Boxplot by MaternalLine #################################
####################################################################################

q <- ggplot(het_data_joined, aes(x = MaternalLineRenamed, y = F, fill = MaternalLineRenamed)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.fill = NA) +
  geom_point(size = 1) +
  theme_bw() +
  labs(title = "Inbreeding Coefficient (F) by Maternal Line", 
       x = "Maternal Line", y = "F") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

q

ggsave("F_boxplot_by_maternalline_named.png", plot = q, width = 10, height = 6, dpi = 300)

# Calculate summary statistics for each maternal line
boxplot_stats <- het_data_joined %>%
  group_by(MaternalLineRenamed) %>%
  summarise(
    Min = min(F, na.rm = TRUE),
    Q1 = quantile(F, 0.25, na.rm = TRUE),
    Median = median(F, na.rm = TRUE),
    Q3 = quantile(F, 0.75, na.rm = TRUE),
    Max = max(F, na.rm = TRUE),
    Mean = mean(F, na.rm = TRUE),
    .groups = "drop"
  )

print(boxplot_stats)  
