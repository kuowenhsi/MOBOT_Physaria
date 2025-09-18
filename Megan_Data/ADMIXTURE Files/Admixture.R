

####################################################################################
############################## Admixture Code ######################################
####################################################################################

library(tidyverse)
library(ggh4x)
library(RColorBrewer)

setwd("~/Desktop/Physaria_Data/VCF-Plink-Remove-Admixture/Admixture_files")

####################################################################################
############################## Individual Plot #####################################
####################################################################################

#### 1) Define files and parameters ####
qfile <- "Physaria_Chr_1_ldpruned.5.Q"
k_val <- str_extract(qfile, "(?<=\\.)(\\d+)(?=\\.Q)")
fam_file <- str_replace(qfile, "\\.\\d+\\.Q", ".fam")

#### 2) Read and clean fam sample names ####
fam_samples <- read_table(fam_file, col_names = FALSE) %>%
  pull(X2) %>%
  tolower() %>%
  trimws()

#### 3) Load and clean metadata ####
metadata <- read.csv("Physaria_MergedData_20240703.csv", check.names = FALSE, stringsAsFactors = FALSE)
colnames(metadata)[1] <- "Sample_Name"
metadata$Sample_Name <- paste0("Physaria_", str_pad(metadata$Sample_Name, 3, pad = "0")) %>%
  tolower() %>%
  trimws()

#### 4) Read Q matrix and join with metadata ####
qmat <- read_table(qfile, col_names = FALSE) %>%
  mutate(Sample_Name = fam_samples) %>%
  left_join(metadata, by = "Sample_Name") %>%
  pivot_longer(cols = starts_with("X"), names_to = "Ancestry", values_to = "Proportion") %>%
  mutate(
    County = ifelse(is.na(County), "Unknown", County),
    MaternalLine = ifelse(is.na(MaternalLine), "Unknown", MaternalLine),
    Latitude = ifelse(is.na(Latitude), -9999, Latitude),
    Ancestry = factor(Ancestry, levels = c("X1", "X7", "X2", "X3", "X6", "X5", "X4", "X8", "X9", "X10"))
  )

#### 5) Replace missing values in key columns ####
qmat <- qmat %>%
  mutate(
    County = ifelse(is.na(County), "Unknown", County),
    MaternalLine = ifelse(is.na(MaternalLine), "Unknown", MaternalLine),
    Latitude = ifelse(is.na(Latitude), -9999, Latitude)
  )

### 7) Define Maternal Line Order and Facet Levels ####
manual_order <- c("2021-1843-2", "2021-1841-1", "2023-0185-2", 
                  "2021-0838-1", "2021-1836-2", "2021-1839-2", 
                  "2021-1838-2", "2018-1133-2")

custom_labels <- c(
  "2021-1841-1" = "Franklin1",
  "2021-1843-2" = "Franklin2",
  "2023-0185-2" = "Posey",
  "2021-0838-1" = "Montgomery",
  "2021-1836-2" = "Cheatham1",
  "2021-1839-2" = "Cheatham2",
  "2021-1838-2" = "Davidson",
  "2018-1133-2" = "Trousdale"
)

qmat <- qmat %>%
  mutate(
    CustomLabel = custom_labels[as.character(MaternalLine)],
    CustomLabel = ifelse(is.na(CustomLabel), "Unknown", CustomLabel)
  )

facet_levels <- qmat %>%
  distinct(CustomLabel, MaternalLine) %>%
  filter(MaternalLine %in% manual_order) %>%
  mutate(MaternalLine = factor(MaternalLine, levels = manual_order)) %>%
  arrange(MaternalLine, CustomLabel) %>%
  transmute(Facet_Label = paste0(CustomLabel, "\n", MaternalLine)) %>%
  pull(Facet_Label)

qmat <- qmat %>%
  mutate(
    Facet_Label = factor(paste0(CustomLabel, "\n", MaternalLine), levels = facet_levels)
  )

cluster_colors <- c(
  "X1" = "#d95f02",  
  "X2" = "#1b9e77", 
  "X3" = "#7570b3",  
  "X4" = "#e6ab02", 
  "X5" = "#e7298a",  
  "X6" = "#666666",  
  "X7" = "#a6cee3",  
  "X8" = "#BC80BD", 
  "X9" = "#1f78b4", 
  "X10" = "#66a61e" 
)

#### 8) Plot ADMIXTURE barplot ####
q <- ggplot(qmat, aes(x = Sample_Name, y = Proportion, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  theme_bw() +
  ggh4x::facet_nested(~ Facet_Label, scales = "free_x", space = "free_x") +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(values = cluster_colors) +
  labs(
    title = paste("ADMIXTURE Plot based on neutral SNPs (K =", k_val, ")"),
    x = NULL,
    y = "Ancestry Proportion"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none",
    strip.text.x = element_text(size = 24, face = "bold")  # <-- this is the key line
  )

print(q)

q <- q + 
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16)
  )

#### 9) Save your Plot ####
ggsave(
  filename = paste0("Physaria_neutral_ADMIXTURE_K_", k_val, ".png"),
  plot = q,
  width = 35,
  height = 6.5,
  dpi = 600, 
  bg = "white"
)

##################################################################################
################################### K = 1-10 #####################################
##################################################################################

metadata <- metadata %>%
  mutate(County = case_when(
    MaternalLine == "2021-1841-1" ~ "Franklin1",
    MaternalLine == "2021-1843-2" ~ "Franklin2",
    MaternalLine == "2021-1836-2" ~ "Cheatham1",
    MaternalLine == "2021-1839-2" ~ "Cheatham2",
    
    TRUE ~ County
  ))

fam <- read_table("Physaria_Chr_1_ldpruned.fam", col_names = FALSE)
samples <- fam %>%
  select(Sample_Name = X2) %>%
  mutate(Sample_Name = tolower(trimws(Sample_Name)))

admix_list <- list()
for (K in 1:10) {
  qfile <- sprintf("Physaria_Chr_1_ldpruned.%d.Q", K)
  q_df <- read.table(qfile, col.names = paste0("Cluster", 1:K))
  
  q_df <- bind_cols(samples, q_df) %>%
    left_join(metadata, by = "Sample_Name") %>%
    mutate(K = K) %>%
    pivot_longer(
      cols = starts_with("Cluster"),
      names_to = "Cluster",
      values_to = "Ancestry"
    )
  
  admix_list[[K]] <- q_df
}

admix_all <- bind_rows(admix_list)

cluster_colors <- c(
  "Cluster1" = "#1b9e77",  
  "Cluster2" = "#d95f02", 
  "Cluster3" = "#7570b3",  
  "Cluster4" = "#e7298a", 
  "Cluster5" = "#66a61e",  
  "Cluster6" = "#e6ab02",  
  "Cluster7" = "#a6cee3",  
  "Cluster8" = "#666666", 
  "Cluster9" = "#1f78b4", 
  "Cluster10" = "#BC80BD" 
)

manual_order <- c(
  "2021-1843-2", "2021-1841-1", "2023-0185-2", 
  "2021-0838-1", "2021-1836-2", "2021-1839-2", 
  "2021-1838-2", "2018-1133-2"
)

# Create facet labels
facet_levels <- admix_all %>%
  distinct(County, MaternalLine) %>%
  filter(MaternalLine %in% manual_order) %>%
  mutate(MaternalLine = factor(MaternalLine, levels = manual_order)) %>%
  arrange(MaternalLine, County) %>%
  transmute(Facet_Label = paste0(County, "\n", MaternalLine)) %>%
  pull(Facet_Label)

# Add factor levels to full df
admix_all <- bind_rows(admix_list) %>%
  mutate(
    MaternalLine = factor(MaternalLine, levels = manual_order),
    Facet_Label = factor(paste0(County, "\n", MaternalLine), levels = facet_levels),
    Sample_Name = factor(Sample_Name, levels = unique(Sample_Name)),
    OriginalCluster = Cluster
  )

admix_all <- admix_all %>%
  mutate(OriginalCluster = Cluster) %>%  # Preserve original labels
  mutate(
    Cluster_for_color = case_when(
      # Swap Cluster1 and Cluster2 for K = 5 and 6
      K %in% c(5,6) & OriginalCluster == "Cluster1" ~ "Cluster2",
      K %in% c(5,6) & OriginalCluster == "Cluster2" ~ "Cluster1",
      
      # For K = 5: swap Cluster4 and Cluster5
      K == 5 & OriginalCluster == "Cluster4" ~ "Cluster5",
      K == 5 & OriginalCluster == "Cluster5" ~ "Cluster4",
      
      # For K = 6
      K == 6 & OriginalCluster == "Cluster5" ~ "Cluster4",
      K == 6 & OriginalCluster == "Cluster6" ~ "Cluster5",
      K == 6 & OriginalCluster == "Cluster4" ~ "Cluster3",
      K == 6 & OriginalCluster == "Cluster3" ~ "Cluster6",
      
      # For K = 7
      K == 7 & OriginalCluster == "Cluster3" ~ "Cluster4",
      K == 7 & OriginalCluster == "Cluster7" ~ "Cluster3",
      K == 7 & OriginalCluster == "Cluster5" ~ "Cluster7",
      K == 7 & OriginalCluster == "Cluster6" ~ "Cluster5",
      K == 7 & OriginalCluster == "Cluster4" ~ "Cluster6",
      K == 7 & OriginalCluster == "Cluster2" ~ "Cluster1",
      K == 7 & OriginalCluster == "Cluster1" ~ "Cluster2",
      
      # For K = 8
      K == 8 & OriginalCluster == "Cluster2" ~ "Cluster4",
      K == 8 & OriginalCluster == "Cluster7" ~ "Cluster2",
      K == 8 & OriginalCluster == "Cluster1" ~ "Cluster7",
      K == 8 & OriginalCluster == "Cluster3" ~ "Cluster1",
      K == 8 & OriginalCluster == "Cluster6" ~ "Cluster3",
      K == 8 & OriginalCluster == "Cluster8" ~ "Cluster6",
      K == 8 & OriginalCluster == "Cluster5" ~ "Cluster8",
      K == 8 & OriginalCluster == "Cluster4" ~ "Cluster5",
      
      # For K = 9
      K == 9 & OriginalCluster == "Cluster3" ~ "Cluster4",
      K == 9 & OriginalCluster == "Cluster1" ~ "Cluster3",
      K == 9 & OriginalCluster == "Cluster4" ~ "Cluster1",
      K == 9 & OriginalCluster == "Cluster8" ~ "Cluster5",
      K == 9 & OriginalCluster == "Cluster5" ~ "Cluster8",
      K == 9 & OriginalCluster == "Cluster9" ~ "Cluster2",
      K == 9 & OriginalCluster == "Cluster7" ~ "Cluster9",
      K == 9 & OriginalCluster == "Cluster2" ~ "Cluster7",
      
      # For K = 10
      K == 10 & OriginalCluster == "Cluster5" ~ "Cluster4",
      K == 10 & OriginalCluster == "Cluster1" ~ "Cluster5",
      K == 10 & OriginalCluster == "Cluster2" ~ "Cluster1",
      K == 10 & OriginalCluster == "Cluster7" ~ "Cluster2",
      K == 10 & OriginalCluster == "Cluster10" ~ "Cluster7",
      K == 10 & OriginalCluster == "Cluster3" ~ "Cluster10",
      K == 10 & OriginalCluster == "Cluster4" ~ "Cluster3",
      K == 10 & OriginalCluster == "Cluster8" ~ "Cluster6",
      K == 10 & OriginalCluster == "Cluster6" ~ "Cluster8",
      
      TRUE ~ OriginalCluster
    )
  )

q <- ggplot(admix_all, aes(x = Sample_Name, y = Ancestry, fill = Cluster_for_color)) +
  geom_bar(stat = "identity", width = 1) +
  facet_grid(K ~ Facet_Label, scales = "free_x", space = "free_x") +
  theme_bw() +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(values = cluster_colors) + 
  labs(
    title = "ADMIXTURE Plot (K = 1 to 10)",
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text.x = element_blank(),       
    axis.text.y = element_blank(),       
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), 
    panel.grid = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  )

q
ggsave("admixture_multik1_10.png", q, width = 16, height = 20, dpi = 300)
