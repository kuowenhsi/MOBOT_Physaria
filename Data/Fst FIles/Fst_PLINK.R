
###########################################################################
########################## Using PLINK2 for Fst ###########################
###########################################################################

library(tidyverse)
library(sf)
library(viridis)

setwd("~/Desktop/Physaria_Data/Fst_Stuff")

###########################################################################
################ Create physaria.pop and fix physaria.psam ################
###########################################################################

# Load and define populations
meta_raw <- read_csv("Physaria_MergedData_20240703.csv")

line_map <- tibble::tibble(
  MaternalLine = c("2021-1843-2", "2023-0185-2", "2021-1838-2", "2021-1836-2", 
                   "2021-0838-1", "2018-1133-2", "2021-1839-2", "2021-1841-1"),
  POP = c("Franklin2", "Posey", "Davidson", "Cheatham1", 
          "Montgomery", "Trousdale", "Cheatham2", "Franklin1")
)

meta <- meta_raw %>%
  mutate(FID = paste0("Physaria_", str_pad(Index, width = 3, pad = "0")),
         IID = FID) %>%
  left_join(line_map, by = "MaternalLine") %>%
  select(FID, IID, POP)

# Check and write PLINK .pop file
stopifnot(!any(is.na(meta$POP)))
write.table(meta, "physaria.pop", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Load existing psam and merge with POP
psam <- read.table("physaria_clean.psam", header = TRUE)
psam <- psam %>% select(FID, IID)
psam_merged <- left_join(psam, meta, by = c("FID", "IID"))

# Check and write updated .psam
stopifnot(!any(is.na(psam_merged$POP)))
write.table(psam_merged, "physaria_clean.psam", quote = FALSE, row.names = FALSE, col.names = TRUE)

###########################################################################
############################# Make Fst Table ##############################
###########################################################################

fst_summary <- read_tsv("physaria_MaternalLine.fst.summary") %>%
  rename(POP1 = `#POP1`, POP2 = POP2, FST = HUDSON_FST)

fst_table_sym <- fst_summary %>%
  bind_rows(fst_summary %>% rename(POP1 = POP2, POP2 = POP1)) %>%
  bind_rows(tibble(POP1 = unique(fst_summary$POP1), POP2 = unique(fst_summary$POP1), FST = 0))

fst_sym <- fst_table_sym %>%
  complete(POP1, POP2) %>%
  mutate(FST = ifelse(POP1 == POP2, 0, FST))

# Hierarchical clustering for population order
fst_matrix <- fst_sym %>%
  pivot_wider(names_from = POP2, values_from = FST) %>%
  column_to_rownames("POP1") %>%
  as.matrix()

diag(fst_matrix) <- 0
ordered_pops <- hclust(as.dist(fst_matrix), method = "average")$labels[
  hclust(as.dist(fst_matrix), method = "average")$order]

fst_sym <- fst_sym %>%
  mutate(POP1 = factor(POP1, levels = ordered_pops),
         POP2 = factor(POP2, levels = ordered_pops))

###########################################################################
################################ Plotting #################################
###########################################################################

q <- ggplot(fst_sym, aes(x = POP2, y = POP1, fill = FST)) +
  geom_tile(aes(fill = FST), color = "grey30", size = 0.5) +
  geom_tile(data = subset(fst_sym, POP1 == POP2), fill = "black", color = "black", size = 0.5) +  
  geom_text(aes(label = round(FST, 2)), na.rm = TRUE, size = 3.5) +
  coord_fixed() +
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(t = 5, r = 10, b = 10, l = 10)
  ) +
  scale_x_discrete(position = "top", expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(9, "RdYlGn")),
    na.value = "black"
  ) +
  labs(title = "Pairwise genetic divergence between populations", fill = "Fst")

q

ggsave("pairwise_fst_maternalline.png", plot = q,
       width = 8,    # width in inches
       height = 8,   # height in inches
       dpi = 300)    # resolution (dots per inch)

###########################################################################
###################### Graphing Fst vs distance ###########################
###########################################################################

coords_sf <- meta_raw %>%
  distinct(MaternalLine, Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(5070)

dist_matrix <- st_distance(coords_sf)
dist_df <- as.data.frame(as.table(dist_matrix)) %>%
  mutate(
    line1 = coords_sf$MaternalLine[as.integer(Var1)],
    line2 = coords_sf$MaternalLine[as.integer(Var2)],
    distance_km = as.numeric(Freq) / 1000
  ) %>%
  filter(line1 != line2) %>%
  select(line1, line2, distance_km)

line_map <- tibble::tibble(
  MaternalLine = c("2021-1843-2", "2023-0185-2", "2021-1838-2", "2021-1836-2", 
                   "2021-0838-1", "2018-1133-2", "2021-1839-2", "2021-1841-1"),
  NewName = c("Franklin2", "Posey", "Davidson", "Cheatham1", 
          "Montgomery", "Trousdale", "Cheatham2", "Franklin1"))
name_map <- setNames(line_map$NewName, line_map$MaternalLine)
dist_df <- dist_df %>%
  mutate(
    line1 = recode(line1, !!!name_map),
    line2 = recode(line2, !!!name_map)
  ) %>%
  rename(
    pop1 = line1,
    pop2 = line2
  )

fst_df <- read_table("physaria_MaternalLine.fst.summary", col_names = TRUE) %>%
  rename(pop1 = `#POP1`, pop2 = POP2, fst = HUDSON_FST)

fst_df <- alphabetize_pairs(fst_df)
alphabetize_pairs <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      pop1_new = min(c(pop1, pop2)),
      pop2_new = max(c(pop1, pop2))
    ) %>%
    ungroup() %>%
    select(-pop1, -pop2) %>%
    rename(pop1 = pop1_new, pop2 = pop2_new)
}
fst_dist_df <- inner_join(fst_df, dist_df, by = c("pop1", "pop2"))

p <- ggplot(fst_dist_df, aes(x = distance_km, y = fst)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    x = "Geographic Distance (km)",
    y = expression(F[ST]),
    title = "FST vs Geographic Distance by Maternal Line"
  ) +
  theme_bw()

p

ggsave("pairwise_fst_by_geo_distance.png", plot = p,
       width = 8,    # width in inches
       height = 8,   # height in inches
       dpi = 300)    # resolution (dots per inch)
