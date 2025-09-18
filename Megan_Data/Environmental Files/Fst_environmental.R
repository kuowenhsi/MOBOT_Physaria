

####################################################################################
######################### Fst: Environmental Associations ##########################
####################################################################################

library(tidyverse)
library(broom)
library(vegan)
library(FactoMineR)
library(ggrepel)
library(ggforce)
library(sp)
library(sf)
library(dplyr)
select <- dplyr::select

setwd("~/Desktop/Physaria_Data/Fst_Stuff/byEnvironmental")

#### 1) Read and process FST data ####
fst_input <- read_tsv("physaria_MaternalLine.fst.summary") %>%
  mutate(linearized_fst = HUDSON_FST / (1 - HUDSON_FST))
colnames(fst_input) <- c("POP1", "POP2", "FST", "L_FST")

#### 2) Calculate quantiles for FST ####
quantile(fst_input$FST)

#### 3) Get unique population names ####
pop_names <- unique(c(fst_input$POP1, fst_input$POP2))

#### 4) Split FST data by population and process outliers ####
fst_by_pop_list <- list()
for (i in pop_names) {
  fst_by_pop_list[[i]] <- fst_input[(fst_input$POP1 == i) | (fst_input$POP2 == i), ]
}
fst_by_pop <- bind_rows(fst_by_pop_list, .id = "REF") %>%
  group_by(REF) %>%
  mutate(OUTLIER = FST > (quantile(FST, probs = 0.75) + 1.5 * IQR(FST))) %>%
  mutate(OUTLIER_LABEL = case_when((OUTLIER == TRUE) & (REF == POP1) ~ POP2,
                                   (OUTLIER == TRUE) & (REF != POP1) ~ POP1,
                                   TRUE ~ as.character(NA))) %>%
  arrange(desc(FST), .by_group = TRUE) %>%
  mutate(count_REF = cumsum(OUTLIER)) %>%
  mutate(LABEL_x = case_when((OUTLIER == TRUE) & (count_REF %% 2 == 1) ~ 0.7,
                             (OUTLIER == TRUE) & (count_REF %% 2 == 0) ~ 1.3,
                             TRUE ~ as.numeric(NA)))

#### 5) Read and process climate data ####
data_input <- read_csv("Physaria_buf_climate_data_20240724.csv")[, c(1:2, 4:38)]
metadata <- read_csv("Physaria_MergedData_20240703.csv")
data_input <- data_input %>%
  left_join(metadata %>% select(Index, Latitude, Longitude), by = "Index")
data_input <- data_input %>%
  select(1:2, Latitude, Longitude, everything())

#### 6) Clean column names ####
colnames(data_input) <- colnames(data_input) %>% str_remove("current_30arcsec_|wc2.1_30s_")
colnames(data_input)[21:39] <- c(
  "Bio1 - Annual Mean Temperature",
  "Bio10 - Mean Temperature of Warmest Quarter",
  "Bio11 - Mean Temperature of Coldest Quarter",
  "Bio12 - Annual Precipitation",
  "Bio13 - Precipitation of Wettest Month",
  "Bio14 - Precipitation of Driest Month",
  "Bio15 - Precipitation Seasonality",
  "Bio16 - Precipitation of Wettest Quarter",
  "Bio17 - Precipitation of Driest Quarter",
  "Bio18 - Precipitation of Warmest Quarter",
  "Bio19 - Precipitation of Coldest Quarter",
  "Bio2 - Mean Diurnal Range",
  "Bio3 - Isothermality",
  "Bio4 - Temperature Seasonality",
  "Bio5 - Max Temperature of Warmest Month",
  "Bio6 - Min Temperature of Coldest Month",
  "Bio7 - Temperature Annual Range",
  "Bio8 - Mean Temperature of Wettest Quarter",
  "Bio9 - Mean Temperature of Driest Quarter"
)

data_input <- data_input %>%
  select(-Index)
data_input <- data_input %>%
  mutate(MaternalLine = case_when(
    MaternalLine == "2021-1841-1" ~ "Franklin1",
    MaternalLine == "2021-1843-2" ~ "Franklin2",
    MaternalLine == "2023-0185-2" ~ "Posey",
    MaternalLine == "2021-0838-1" ~ "Montgomery",
    MaternalLine == "2021-1836-2" ~ "Cheatham1",
    MaternalLine == "2021-1839-2" ~ "Cheatham2",
    MaternalLine == "2021-1838-2" ~ "Davidson",
    MaternalLine == "2018-1133-2" ~ "Trousdale",
    TRUE ~ MaternalLine  
  ))

#### 7) Inspect unique values for growing degree days ####
unique(data_input$growingDegDays5)

#### 8) Perform PCA on climate data ####
clim_PCA <- PCA(data_input, quali.sup = 1, quanti.sup = 2:3)
clim_PCA$eig
clim_PCA$var$coord
PCA_cor <- as.data.frame(clim_PCA$var$cor) %>%
  rownames_to_column("env") %>%
  arrange(desc(abs(Dim.1))) %>%
  mutate(ID = seq(n()))

#### 9) Extract PCA individual coordinates and combine with data ####
clim_PCA_indv <- as.data.frame(clim_PCA$ind$coord) %>%
  bind_cols(data_input[, 2:3])

#### 10) Aggregate PCA results by population ####
clim_PCA_Pop <- as.data.frame(clim_PCA$quali.sup$coord) %>%
  rownames_to_column("MaternalLine") %>%
  bind_cols(data_input[, 1:3] %>% group_by(MaternalLine) %>% summarise_all(mean) %>% select(2:3))

#### 11) Combine climate data with PCA results ####
clim_input <- bind_cols(data_input, clim_PCA$ind$coord) %>%
  group_by(MaternalLine) %>%
  summarise_all(mean)

#### 12) Convert to spatial format ####
clim_input_sf <- st_as_sf(clim_input, coords = c("Longitude", "Latitude"), crs = 4326)

#### 13) Calculate geographic distances ####
geo_dist <- st_distance(clim_input_sf, clim_input_sf)
rownames(geo_dist) <- clim_input$MaternalLine
colnames(geo_dist) <- clim_input$MaternalLine
geo_dist_df <- tidy(as.dist(geo_dist, diag = FALSE, upper = FALSE))
colnames(geo_dist_df) <- c("POP1", "POP2", "geo_dist")

#### 14) Calculate climate distances ####
clim_dist <- bind_cols(lapply(clim_input[4:39], function(x) as.numeric(dist(scale(x)))))

#### 15) Combine geographic, climate, and FST data ####
geo_clim_dist <- bind_cols(geo_dist_df, clim_dist) %>%
  left_join(fst_input, by = c("POP1", "POP2"))

#### 16) Prepare FST distance matrices ####
fst_matrix <- matrix(NA, nrow = 8, ncol = 8)
fst_matrix[lower.tri(fst_matrix)] <- geo_clim_dist$FST
fst_dist <- as.dist(fst_matrix, diag = FALSE, upper = FALSE)

L_fst_matrix <- matrix(NA, nrow = 8, ncol = 8)
L_fst_matrix[lower.tri(L_fst_matrix)] <- geo_clim_dist$L_FST
L_fst_dist <- as.dist(L_fst_matrix, diag = FALSE, upper = FALSE)

###########################################################################

#### 17) Define functions for Mantel tests ####
get_mantel_result <- function(x) {
  result <- mantel(L_fst_dist, dist(x), permutations = 50e3)
  return(tibble(r = result$statistic, p_value = result$signif))
}

get_mantel_result_geoDist <- function(x) {
  result <- mantel(L_fst_dist, geo_dist, permutations = 50e3)
  return(tibble(r = result$statistic, p_value = result$signif, env_var = "geo_dist"))
}

get_mantel.partial_result <- function(x) {
  result <- mantel.partial(L_fst_dist, dist(x), geo_dist, permutations = 50e3, parallel = 8)
  return(tibble(r = result$statistic, p_value = result$signif))
}

#### 18) Perform Mantel tests and save results ####
pca_dims <- colnames(clim_PCA$ind$coord)
clim_vars_only <- clim_input %>%
  select(-all_of(pca_dims))
mantel_results <- bind_rows(lapply(clim_vars_only[4:ncol(clim_vars_only)], get_mantel_result)) %>%
  mutate(env_var = colnames(clim_vars_only)[4:ncol(clim_vars_only)]) %>%
  bind_rows(get_mantel_result_geoDist())

#### 19) Annotate significance categories ####
mantel_results_sig <- mantel_results %>%
  mutate(sig_color = case_when(
    p_value >= 0.05 ~ "p >= 0.05",
    p_value < 0.05 & p_value >= 0.001 ~ "p < 0.05",
    p_value < 0.001 ~ "p < 0.001"
  ))

#### 20) Save and read if needed ####
write_excel_csv(mantel_results_sig, "mantel_results_sig_20230621.csv")
mantel_results_sig <- read_csv("mantel_results_sig_20230621.csv")

###########################################################################

# Create a significance category column
mantel_results_sig <- mantel_results_sig %>%
  mutate(Significance = case_when(
    p_value < 0.001 ~ "p < 0.001",
    p_value < 0.05 ~ "p < 0.05",
    TRUE ~ "p ≥ 0.05"
  ))

# Drop NAs + reorder + define custom colors
mantel_results_sig <- mantel_results_sig %>%
  drop_na() %>%  
  mutate(
    Variable = fct_reorder(env_var, r, .desc = TRUE),
    Significance = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.05 ~ "p < 0.05",
      TRUE ~ "p ≥ 0.05"
    ))

# Define custom color mapping
significance_colors <- c(
  "p ≥ 0.05" = "gray60",
  "p < 0.05" = "orange",
  "p < 0.001" = "red"
)

# Plot
p <- ggplot(mantel_results_sig, aes(x = r, y = Variable)) +
  geom_segment(aes(x = 0, xend = r, yend = Variable), color = "black", linewidth = .5) +
  geom_point(aes(color = Significance), size = 2) +
  scale_color_manual(values = significance_colors) +
  scale_y_discrete(limits = rev) +
  theme_bw(base_size = 10) +
  labs(
    x = "Mantel r",
    y = NULL,
    title = NULL,
    color = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.76, 0.08),   # adjust x,y inside [0,1]
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )
p

ggsave("mantel_plot.png", plot = p, width = 5, height = 8, dpi = 300)

###########################################################################
# Bio17 - Precipitation of Driest Quarter vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio17 - Precipitation of Driest Quarter`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 2, y = 0.14, label = "Mantel r = 0.782\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Precipitation of Driest Quarter (mm)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio17PrecipDriestQuarter.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio14 - Precipitation of Driest Month vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio14 - Precipitation of Driest Month`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.63, y = 0.14, label = "Mantel r = 0.775\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Precipitation of Driest Month (mm)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio14PrecipDriestMonth.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio12 - Annual Precipitation vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio12 - Annual Precipitation`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.57, y = 0.14, label = "Mantel r = 0.760\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Annual Precipitation (mm)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio12AnnualPrecipitation.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio19 - Precipitation of Coldest Quarter vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio19 - Precipitation of Coldest Quarter`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.4, y = 0.14, label = "Mantel r = 0.748\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Precipitation of Coldest Quarter (mm)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio19PrecipColdestQuarter.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# maxTempColdest days vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = maxTempColdest)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.4, y = 0.14, label = "Mantel r = 0.684\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Maximum Temp. of the Coldest Month (°C*10)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("maxTempColdest.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# PETColdestQuarter vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = PETColdestQuarter)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.37, y = 0.14, label = "Mantel r = 0.662\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Mean Monthly PET Coldest Quarter (" * "mm " * month^{-1} * ")")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("PETColdestQuarter.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# annualPET vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = annualPET)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.55, y = 0.14, label = "Mantel r = 0.659\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Annual Potential Evapotranspiration (mm " * yr^{-1} * ")")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("annualPET.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio11 - Mean Temperature of Coldest Quarter vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio11 - Mean Temperature of Coldest Quarter`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.45, y = 0.14, label = "Mantel r = 0.627\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Mean Temp. of Coldest Quarter (°C)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio11MeanTemperatureColdestQuarter.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio4 - Temperature Seasonality vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio4 - Temperature Seasonality`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.8, y = 0.14, label = "Mantel r = 0.624\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Temperature Seasonality (%)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio4TempSeasonality.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# growingDegDays0 days vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = growingDegDays0)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.5, y = 0.14, label = "Mantel r = 0.622\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Growing Degree Days (>0"*degree*C*") (standardized)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("growingDegDays0.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# PETWarmestQuarter vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = PETWarmestQuarter)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 2.1, y = 0.14, label = "Mantel r = 0.580\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Mean Monthly PET of Warmest Quarter (" * "mm " * month^{-1} * ")")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("PETWarmestQuarter.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio3 - Isothermality vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio3 - Isothermality`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.8, y = 0.14, label = "Mantel r = 0.559\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Isothermality (%)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio3Isothermality.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio13 - Precipitation of Wettest Month vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio13 - Precipitation of Wettest Month`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.64, y = 0.14, label = "Mantel r = 0.554\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Precipitation of Wettest Month (mm)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio13PrecipWettestMonth.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio15 - Precipitation Seasonality vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio15 - Precipitation Seasonality`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 2.3, y = 0.14, label = "Mantel r = 0.461\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Precipitation Seasonality (%)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio15PrecipSeasonality.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio6 - Min Temperature of Coldest Month vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio6 - Min Temperature of Coldest Month`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.57, y = 0.14, label = "Mantel r = 0.531\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Min Temperature of Coldest Month (°C*10)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio6MinTempColdestMonth.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio16 - Precipitation of Wettest Quarter vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio16 - Precipitation of Wettest Quarter`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.7, y = 0.14, label = "Mantel r = 0.522\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Precipitation of Wettest Quarter (mm)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio16PrecipWettestQuarter.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio2 - Mean Diurnal Range vs. FST
p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio2 - Mean Diurnal Range`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.9, y = 0.14, label = "Mantel r = 0.497\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Mean Diurnal Range (°C)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio2MeanDiurnalRange.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# climaticMoistureIndex days vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = climaticMoistureIndex)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.9, y = 0.14, label = "Mantel r = 0.489\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Climatic Moisture Index")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("climaticMoistureIndex.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Plot geographic distance vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = geo_dist / 1000)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 195, y = 0.14, label = "Mantel r = 0.476\np = 0.013", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab("Geographic distance (km)") +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("GeoDist.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio1 - Annual Mean Temperature vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio1 - Annual Mean Temperature`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.57, y = 0.14, label = "Mantel r = 0.432\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Annual Mean Temperature (°C)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio1AnnualMeanTemp.png", plot = p, width = 4, height = 4, dpi = 900)

###########################################################################
# Bio7 - Temperature Annual Range vs. FST

p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio7 - Temperature Annual Range`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 1.6, y = 0.14, label = "Mantel r = 0.414\np < 0.05", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Temperature Annual Range (°C)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("Bio7TemperatureAnnualRange.png", plot = p, width = 4, height = 4, dpi = 900)

