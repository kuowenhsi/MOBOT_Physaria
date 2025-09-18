

####################################################################################
##################################### PCA Plots ####################################
####################################################################################

library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(scales)

setwd("~/Desktop/Physaria_Data/VCF-Plink-Remove-Admixture/Admixture_files")

#### 1) Load PCA data ####
pca <- read.table("Physaria_Chr_1_pca.eigenvec", header = FALSE)

colnames(pca) <- c("FID", "IID", paste0("PC", 1:(ncol(pca) - 2)))

pca$IID <- sub("^Physaria_", "", pca$IID)

pca$IID <- as.numeric(pca$IID)

#### 2) Load metadata ####
meta <- read.csv("Physaria_MergedData_20240703.csv", check.names = FALSE, stringsAsFactors = FALSE)

colnames(meta)[1] <- "SampleID"

meta$SampleID <- as.numeric(meta$SampleID)

#### 3) Merge PCA and Meta data ####
pca_data <- merge(pca, meta, by.x = "IID", by.y = "SampleID")

eigenval <- scan("Physaria_Chr_1_pca.eigenval")
pc_percent <- eigenval / sum(eigenval) * 100

####################################################################################
############################### Plot by Latitude ###################################
####################################################################################

a <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Latitude)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  theme_bw() +
  labs(
    title = "PCA Colored by Latitude",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
a

ggsave("pca_by_Latitude.png", plot = a,
       width = 8, height = 6, dpi = 300, units = "in")

####################################################################################
################################## Plot by State ###################################
####################################################################################

state_colors <- c(
  "Tennessee" = "#377EB8",  
  "Kentucky"  = "#4DAF4A", 
  "Indiana"   = "#E41A1C"   
)

b <- ggplot(pca_data, aes(x = PC1, y = PC2, color = State)) +
  geom_point(size = 3) +
  scale_color_manual(values = state_colors) +
  theme_bw() +
  labs(
    title = "PCA Colored by State", color = "State",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
b

ggsave("pca_by_State.png", plot = b,
       width = 8, height = 6, dpi = 300, units = "in")

####################################################################################
################################# Plot by County ###################################
####################################################################################

county_colors <- c(
  "Cheatham"   = "#984EA3", 
  "Davidson"   = "#FF7F00", 
  "Franklin"   = "#4DAF4A", 
  "Montgomery" = "#377EB8",  
  "Posey"      = "#E41A1C",  
  "Trousdale"  = "#E7298A"  
)

county_shapes <- c(
  "Cheatham" = 17,     
  "Davidson" = 20,
  "Franklin" = 18,
  "Montgomery" = 3,
  "Posey" = 15,
  "Trousdale" = 1     
)

c <- ggplot(pca_data, aes(x = PC1, y = PC2, color = County, shape = County)) +
  geom_point(size = 3) +
  scale_color_manual(values = county_colors) +
  scale_shape_manual(values = county_shapes) +
  theme_bw() +
  labs(
    title = "PCA colored by County", color = "County",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )

c

ggsave("pca_by_County.png", plot = c,
       width = 8, height = 6, dpi = 300, units = "in")


####################################################################################
############################## Plot by MaternalLine ################################
####################################################################################

maternal_colors <- c(
  "Cheatham1"   = "#984EA3",
  "Cheatham2" = "#A1CAF1",
  "Davidson"   = "#F38400", 
  "Franklin1"   = "#4DAF4A",
  "Franklin2" = "#F6A600",
  "Montgomery" = "#377EB8",  
  "Posey"      = "#E41A1C",  
  "Trousdale"  = "#E7298A"  
)

maternal_line_labels <- c(
  "2021-1843-2" = "Franklin2",
  "2023-0185-2" = "Posey",
  "2021-1838-2" = "Davidson",
  "2021-1836-2" = "Cheatham1",
  "2021-0838-1" = "Montgomery",
  "2018-1133-2" = "Trousdale",
  "2021-1839-2" = "Cheatham2",
  "2021-1841-1" = "Franklin1"
)

# Apply relabeling and preserve order
pca_data <- pca_data %>%
  mutate(MaternalLine = factor(recode(MaternalLine, !!!maternal_line_labels),
                               levels = maternal_line_labels))

d <- ggplot(pca_data, aes(x = PC1, y = PC2, color = MaternalLine)) +
  geom_point(size = 3) +
  scale_color_manual(values = maternal_colors) +
  theme_bw() +
  labs(
    title = "PCA colored by Maternal Line", color = "Maternal Line",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
d

ggsave("pca_by_Maternal.png", plot = d,
       width = 8, height = 6, dpi = 300, units = "in")

####################################################################################
# Posey, Franklin1, and Franklin2 Maternal Lines

# Define counties to exclude 
excluded_maternal_a <- c("cheatham1", "cheatham2", "davidson", "montgomery", "trousdale")

# Filter
pca_data_a <- pca_data %>%
  filter(!tolower(MaternalLine) %in% excluded_maternal_a)

e <- ggplot(pca_data_a, aes(x = PC1, y = PC2, color = MaternalLine)) +
  geom_point(size = 3) +
  scale_color_manual(values = maternal_colors) +
  theme_bw() +
  labs(
    title = "PCA colored by Maternal Line", color = "Maternal Line",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
e

ggsave("pca_by_MaternalA.png", plot = e,
       width = 8, height = 6, dpi = 300, units = "in")

####################################################################################
# Cheatham1, Cheatham2, and Davidson Maternal Lines

# Define counties to exclude 
excluded_maternal_b <- c("franklin1", "franklin2", "posey", "montgomery", "trousdale")

# Filter
pca_data_b <- pca_data %>%
  filter(!tolower(MaternalLine) %in% excluded_maternal_b)

f <- ggplot(pca_data_b, aes(x = PC1, y = PC2, color = MaternalLine)) +
  geom_point(size = 3) +
  scale_color_manual(values = maternal_colors) +
  theme_bw() +
  labs(
    title = "PCA colored by Maternal Line", color = "Maternal Line",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
f

ggsave("pca_by_MaternalB.png", plot = f,
       width = 8, height = 6, dpi = 300, units = "in")

####################################################################################
############################# Plot by 1st Leaf Date ################################
####################################################################################

pca_data$FirstLeafDate <- as.Date(pca_data$FirstLeafDate, format = "%m/%d/%y")

# Check
str(pca_data$FirstLeafDate)      # Should say "Date"
summary(pca_data$FirstLeafDate)  # Should show actual min/max dates
head(pca_data$FirstLeafDate)     # Should show as i.e. "2024-03-30"

# Convert dates to numeric internally for color scale
pca_data$FirstLeafDate_numeric <- as.numeric(pca_data$FirstLeafDate)

# Create breaks as Dates again
breaks_dates <- as.Date(pretty(pca_data$FirstLeafDate_numeric, n = 5), origin = "1970-01-01")

g <- ggplot(pca_data, aes(x = PC1, y = PC2, color = FirstLeafDate_numeric)) +
  geom_point(size = 3) +
  scale_color_gradientn(
    colors = viridis::viridis(256, option = "plasma"),
    breaks = as.numeric(breaks_dates),
    labels = format(breaks_dates, "%Y-%m-%d")
  ) +
  theme_bw() +
  labs(
    title = "PCA Colored by First Leaf Date",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)"),
    color = "First Leaf Date"
  ) +
  theme(
    legend.title = element_text(margin = margin(t = 15, b = 10)),  
    legend.text = element_text(margin = margin(l = 5))
  )
g

ggsave("pca_by_FirstLeaf.png", plot = g,
       width = 8, height = 6, dpi = 300, units = "in")

unique(pca_data$State)
Kentucky_subset <- pca_data %>% filter(State == "Kentucky")
Indiana_subset <- pca_data %>% filter(State == "Indiana")
Tennessee_subset <- pca_data %>% filter(State == "Tennessee")

h <- ggplot(Kentucky_subset, aes(x = PC1, y = PC2, color = FirstLeafDate_numeric)) +
  geom_point(size = 3) +
  scale_color_gradientn(
    colors = viridis::viridis(256, option = "plasma"),
    breaks = as.numeric(breaks_dates),
    labels = format(breaks_dates, "%Y-%m-%d")
  ) +
  theme_bw() +
  labs(
    title = "PCA of Kentucky Colored by First Leaf Date",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)"),
    color = "First Leaf Date"
  ) +
  theme(
    legend.title = element_text(margin = margin(t = 15, b = 10)),  
    legend.text = element_text(margin = margin(l = 5))
  )
h

ggsave("pca_by_FirstLeafKentucky.png", plot = h,
       width = 8, height = 6, dpi = 300, units = "in")

i <- ggplot(Indiana_subset, aes(x = PC1, y = PC2, color = FirstLeafDate_numeric)) +
  geom_point(size = 3) +
  scale_color_gradientn(
    colors = viridis::viridis(256, option = "plasma"),
    breaks = as.numeric(breaks_dates),
    labels = format(breaks_dates, "%Y-%m-%d")
  ) +
  theme_bw() +
  labs(
    title = "PCA of Indiana Colored by First Leaf Date",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)"),
    color = "First Leaf Date"
  ) +
  theme(
    legend.title = element_text(margin = margin(t = 15, b = 10)),  
    legend.text = element_text(margin = margin(l = 5))
  )
i

ggsave("pca_by_FirstLeafIndiana.png", plot = i,
       width = 8, height = 6, dpi = 300, units = "in")

j <- ggplot(Tennessee_subset, aes(x = PC1, y = PC2, color = FirstLeafDate_numeric)) +
  geom_point(size = 3) +
  scale_color_gradientn(
    colors = viridis::viridis(256, option = "plasma"),
    breaks = as.numeric(breaks_dates),
    labels = format(breaks_dates, "%Y-%m-%d")
  ) +
  theme_bw() +
  labs(
    title = "PCA of Tennessee Colored by First Leaf Date",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)"),
    color = "First Leaf Date"
  ) +
  theme(
    legend.title = element_text(margin = margin(t = 15, b = 10)),  
    legend.text = element_text(margin = margin(l = 5))
  )
j

ggsave("pca_by_FirstLeafTennessee.png", plot = j,
       width = 8, height = 6, dpi = 300, units = "in")

####################################################################################
########################## PCA and Dominant Ancestry Plot ##########################
####################################################################################

#### 1) Load PCA data ####
pca <- read.table("Physaria_Chr_1_pca.eigenvec", header = FALSE)
colnames(pca) <- c("FID", "IID", paste0("PC", 1:(ncol(pca) - 2)))

#### 2) Load metadata ####
meta <- read.csv("Physaria_MergedData_20240703.csv", check.names = FALSE, stringsAsFactors = FALSE)
colnames(meta)[1] <- "SampleID"
meta$SampleID <- as.numeric(meta$SampleID)

#### 3) Filter just needed columns for merge ####
meta <- meta %>%
  mutate(Sample_Name = paste0("Physaria_", str_pad(SampleID, width = 3, pad = "0")))
meta_subset <- meta %>%
  select(Sample_Name, MaternalLine, County)

#### 4) Merge PCA and metadata  ####
pca_data <- pca %>%
  mutate(Sample_Name = as.character(IID)) %>%
  left_join(meta_subset, by = "Sample_Name")

#### 5) Get variance explained by PCs ####
eigenval <- scan("Physaria_Chr_1_pca.eigenval")
pc_percent <- eigenval / sum(eigenval) * 100

#### 6) Load Q matrix ####
K <- 5
qmat <- read.table("Physaria_Chr_1_ldpruned.5.Q", header = FALSE)

#### 7) Name ancestry columns ####
colnames(qmat) <- paste0("X", 1:K)
qmat$Sample_Name <- as.character(pca$IID)

#### 8) Pivot longer ####
qmat_long <- qmat %>%
  pivot_longer(cols = starts_with("X"), names_to = "Ancestry", values_to = "Proportion")

#### 9) Convert to wide format ####
qmat_wide <- qmat_long %>%
  select(Sample_Name, Ancestry, Proportion) %>%
  pivot_wider(names_from = Ancestry, values_from = Proportion)

#### 10) Identify dominant ancestry ####
ancestry_cols <- paste0("X", 1:K)
qmat_wide <- qmat_wide %>%
  mutate(Dominant_Ancestry = ancestry_cols[max.col(select(., all_of(ancestry_cols)), ties.method = "first")])

#### 11) Subset metadata to only samples in qmat ####
meta_subset <- meta %>%
  filter(Sample_Name %in% qmat_wide$Sample_Name) %>%
  select(SampleID, Sample_Name, County)

#### 12) Join metadata into qmat ####
qmat_wide <- left_join(qmat_wide, meta_subset, by = "Sample_Name")

#### 13) Detect ancestry columns and find dominant ancestry ####
ancestry_cols <- grep("^X\\d+$", names(qmat_wide), value = TRUE)
qmat_wide <- qmat_wide %>%
  mutate(Dominant_Ancestry = ancestry_cols[max.col(select(., all_of(ancestry_cols)), ties.method = "first")])

#### 14) Match IID name ####
pca_data <- pca_data %>%
  mutate(Sample_Name = as.character(IID))

#### 15) Final join ####
pca_plot_data <- left_join(pca_data, qmat_wide, by = "Sample_Name")

#### 16) Fix county columns ####
pca_plot_data <- pca_plot_data %>%
  select(-matches("^County(\\.x)?$"))
if ("County.y" %in% colnames(pca_plot_data)) {
  pca_plot_data <- pca_plot_data %>%
    rename(County = County.y)
}
if ("County.x" %in% colnames(pca_plot_data)) {
  pca_plot_data <- pca_plot_data %>%
    select(-County.x)
}

#### 17) Double check ####
print(colnames(pca_plot_data))
stopifnot("County" %in% colnames(pca_plot_data))

#### 18) Plot PCA colored by dominant ancestry and shaped by County ####
ancestry_colors <- c(
  "X1" = "#d95f02", #posey
  "X2" = "#1b9e77", #montgomery
  "X3" = "#7570b3", #trousdale
  "X4" = "#e6ab02", #mostly davidson/cheatham
  "X5" = "#e7298a", #franklin
  "X6" = "#66a61e", #mostly cheatham
  "X7" =  "#a6cee3" #mostly davidson/cheatham
)

#### 19) Make sure County names are lowercase to match the shape names ####
pca_plot_data$County <- tolower(pca_plot_data$County)

#### 20) Rename Maternal Line ####
maternal_line_labels <- c(
  "2021-1841-1" = "Franklin1",
  "2021-1843-2" = "Franklin2",
  "2023-0185-2" = "Posey",
  "2021-0838-1" = "Montgomery",
  "2021-1836-2" = "Cheatham1",
  "2021-1839-2" = "Cheatham2",
  "2021-1838-2" = "Davidson",
  "2018-1133-2" = "Trousdale"
)

#### 21) Apply relabeling ####
pca_plot_data <- pca_plot_data %>%
  mutate(MaternalLine = factor(recode(MaternalLine, !!!maternal_line_labels),
                               levels = maternal_line_labels))

#### 22) Define shapes for all counties ####
maternal_shapes <- c(
  "Cheatham1" = 17,
  "Cheatham2" = 6,
  "Davidson" = 20,
  "Franklin1" = 24,
  "Franklin2" = 18,
  "Montgomery" = 8,
  "Posey" = 22,
  "Trousdale" = 1     
)

k <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, color = Dominant_Ancestry, shape = MaternalLine)) +
  geom_point(size = 3) + 
  theme_bw() +
  scale_color_manual(values = ancestry_colors) +
  scale_shape_manual(values = maternal_shapes) +
  theme(legend.position = "right", legend.title = element_text(size = 14), legend.text = element_text(size = 10)) +
  guides(color = "none") +
  labs(
    title = "PCA colored by Dominant Ancestry, shaped by Maternal Line",
    color = "Dominant Ancestry",
    shape = "Maternal Line",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
k

ggsave("pca_dominant_ancestry_by_maternal_K5.png", plot = k,
       width = 8, height = 6, dpi = 300, units = "in")

##################################################################################
############################ Only Posey and Franklin #############################
##################################################################################
# Posey, Franklin1, and Franklin2 Maternal Lines

# Define counties to exclude 
excluded_maternal_a <- c("cheatham1", "cheatham2", "davidson", "montgomery", "trousdale")

# Filter
pca_plot_data_a <- pca_plot_data %>%
  filter(!tolower(MaternalLine) %in% excluded_maternal_a)

l <- ggplot(pca_plot_data_a, aes(x = PC1, y = PC2, color = Dominant_Ancestry, shape = MaternalLine)) +
  geom_point(size = 3) + 
  theme_bw() +
  scale_color_manual(values = ancestry_colors) +
  scale_shape_manual(values = maternal_shapes) +
  labs(
    title = "PCA colored by Dominant Ancestry, shaped by Maternal Line",
    color = "Dominant Ancestry",
    shape = "Maternal Line",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
l

ggsave("pca_dominant_ancestry_by_franklinposey_K7.png", plot = l,
       width = 8, height = 6, dpi = 300, units = "in")

##################################################################################
########################### Only X4, X5 ###########################
##################################################################################
# X4, X6, X7 Maternal Lines

# Define counties to exclude 
excluded_ancestry_c <- c("X1", "X2", "X5")

# Filter
pca_plot_data_c <- pca_plot_data %>%
  filter(!Dominant_Ancestry %in% excluded_ancestry_c)

m <- ggplot(pca_plot_data_c, aes(x = PC1, y = PC2, color = Dominant_Ancestry, shape = MaternalLine)) +
  geom_point(size = 3) + 
  theme_bw() +
  scale_color_manual(values = ancestry_colors) +
  scale_shape_manual(values = maternal_shapes) +
  theme(legend.position = "right", legend.title = element_text(size = 14), legend.text = element_text(size = 10)) +
  labs(
    title = "PCA colored by Dominant Ancestry Line's X3 and X4",
    color = "Dominant Ancestry",
    shape = "Maternal Line",
    x = paste0("PC1 (", round(pc_percent[1], 1), "%)"),
    y = paste0("PC2 (", round(pc_percent[2], 1), "%)")
  )
m

ggsave("pca_dominant_ancestry_by_X34_K5.png", plot = m,
       width = 8, height = 6, dpi = 300, units = "in")
