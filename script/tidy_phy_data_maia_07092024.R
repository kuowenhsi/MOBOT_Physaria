
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(lme4)

setwd("C:/Users/maial/Downloads/MBG REU/MOBOT_Physaria")

phy_tidy_data <- read_csv("data/physaria_buf_climate_data_20240717_l.csv")
phy_climate_data <- read_csv("data/physaria_buf_climate_data_20240717.csv")

unique(phy_tidy_data$variable_name)

phy_climate_data_uni <- phy_climate_data %>%
  mutate(uni_MaternalLine = paste0(Site, as.character(MaternalLine)))

# leafArea.10

## Use this for heritability
h_area <- lmer(leafArea.10 ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_area)
h_area_table <- as_tibble(VarCorr(h_area))
h_area_table$vcov[[1]]/sum(h_area_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_area_anova <- lm(leafArea.10 ~ Site, data = phy_climate_data_uni)
site_area_anova
summary(site_area_anova)
anova(site_area_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_area_anova <- lm(leafArea.10 ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_area_anova)
anova(maternal_area_anova)

# leafLong.10

## Use this for heritability
h_lflong <- lmer(as.numeric(leafLong.10) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_lflong)
h_lflong_table <- as_tibble(VarCorr(h_lflong))
h_lflong_table$vcov[[1]]/sum(h_lflong_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_lflong_anova <- lm(as.numeric(leafLong.10) ~ Site, data = phy_climate_data_uni)
site_lflong_anova
summary(site_lflong_anova)
anova(site_lflong_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_lflong_anova <- lm(as.numeric(leafLong.10) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_lflong_anova)
anova(maternal_lflong_anova)

## Use this for heritability
h_leaflong <- lmer(as.numeric(leafLong.10) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_leaflong)
h_leaflong_table <- as_tibble(VarCorr(h_leaflong))
h_leaflong_table$vcov[[1]]/sum(h_leaflong_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
h_site_lflong <- lmer(as.numeric(leafLong.10) ~ Site + (1|Site/uni_MaternalLine), data = phy_climate_data_uni)
h_site_lflong
summary(h_site_lflong)
anova(h_site_lflong)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
h_maternal_lflong <- lm(as.numeric(leafLong.10) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(h_maternal_lflong)
anova(h_maternal_lflong)
# add all values in variance column
# FlowerHead:MaternalLine or MaternalLine as numerator
summary(h_area)

# numRos.1

h_numRos <- lmer(as.numeric(numRos.1) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_numRos)
h_numRos_table <- as_tibble(VarCorr(h_numRos))
h_numRos_table$vcov[[1]]/sum(h_numRos_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_numRos_anova <- lm(as.numeric(numRos.1) ~ Site, data = phy_climate_data_uni)
site_numRos_anova
summary(site_numRos_anova)
anova(site_numRos_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_numRos_anova <- lm(as.numeric(numRos.1) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_numRos_anova)
anova(maternal_numRos_anova)
# add all values in variance column
# FlowerHead:MaternalLine or MaternalLine as numerator


## numStem.1 

h_numStem <- lmer(as.numeric(numStem.1) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_numStem)
h_numStem_table <- as_tibble(VarCorr(h_numStem))
h_numStem_table$vcov[[1]]/sum(h_numStem_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_numStem_anova <- lm(as.numeric(numStem.1) ~ Site, data = phy_climate_data_uni)
site_numStem_anova
summary(site_numStem_anova)
anova(site_numStem_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_numStem_anova <- lm(as.numeric(numStem.1) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_numStem_anova)
anova(maternal_numStem_anova)
# add all values in variance column
# FlowerHead:MaternalLine or MaternalLine as numerator


## stemDia.1

h_stemDia <- lmer(as.numeric(stemDia.1) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_stemDia)
h_stemDia_table <- as_tibble(VarCorr(h_stemDia))
h_stemDia_table$vcov[[1]]/sum(h_stemDia_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_stemDia_anova <- lm(as.numeric(stemDia.1) ~ Site, data = phy_climate_data_uni)
site_stemDia_anova
summary(site_stemDia_anova)
anova(site_stemDia_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_stemDia_anova <- lm(as.numeric(stemDia.1) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_stemDia_anova)
anova(maternal_stemDia_anova)

## stemLength.1

h_stemLength <- lmer(as.numeric(stemLength.1) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_stemLength)
h_stemLength_table <- as_tibble(VarCorr(h_stemLength))
h_stemLength_table$vcov[[1]]/sum(h_stemLength_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_stemLength_anova <- lm(as.numeric(stemLength.1) ~ Site, data = phy_climate_data_uni)
site_stemLength_anova
summary(site_stemLength_anova)
anova(site_stemLength_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = leafArea.10))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_stemLength_anova <- lm(as.numeric(stemLength.1) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_stemLength_anova)
anova(maternal_stemLength_anova)

## numLeaf.1

h_numLeaf <- lmer(as.numeric(numLeaf.1) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_numLeaf)
h_numLeaf_table <- as_tibble(VarCorr(h_numLeaf))
h_numLeaf_table$vcov[[1]]/sum(h_numLeaf_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_numLeaf_anova <- lm(as.numeric(numLeaf.1) ~ Site, data = phy_climate_data_uni)
site_numLeaf_anova
summary(site_numLeaf_anova)
anova(site_numLeaf_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = as.numeric(numLeaf.1)))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_numLeaf_anova <- lm(as.numeric(numLeaf.1) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_numLeaf_anova)
anova(maternal_numLeaf_anova)

## leafWide.10

h_leafWide <- lmer(as.numeric(leafWide.10) ~ (1|uni_MaternalLine), data = phy_climate_data_uni)
summary(h_leafWide)
h_leafWide_table <- as_tibble(VarCorr(h_leafWide))
h_leafWide_table$vcov[[1]]/sum(h_leafWide_table$vcov)

## Use this to investigate Site's effect on trait
## tell the model there are siblings. Samples are NOT independent from each others
site_lfWide_anova <- lm(as.numeric(leafWide.10) ~ Site, data = phy_climate_data_uni)
site_lfWide_anova
summary(site_lfWide_anova)
anova(site_lfWide_anova)

ggplot(data = phy_climate_data_uni, aes(x = reorder(Site, wc2.1_30s_bio_1), y = as.numeric(numLeaf.1)))+
  geom_boxplot()+
  geom_point(aes(color = as.character(MaternalLine)), position = position_jitter(width = 0.2))+
  theme(legend.position = "none")

## Use this to investigate MaternalLine's effect on trait
maternal_lfWide_anova <- lm(as.numeric(leafWide.10) ~ uni_MaternalLine, data = phy_climate_data_uni)
summary(maternal_lfWide_anova)
anova(maternal_lfWide_anova)

# Create the boxplot and add the ANOVA results
p <- ggplot(data = phy_climate_data, aes(x = reorder(Site, wc2.1_30s_bio_1), y = as.numeric(numRos.1))) +
  stat_summary(geom = "col",  fun = "mean",fill = "deeppink", color = "black") +
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))})+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Site (sorted by AMT)") +
  scale_y_continuous(name = "Number of Rosettes Counted at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5))

print(p)

p1 <- ggplot(data = phy_climate_data, aes(x = reorder(Site, wc2.1_30s_bio_1), y = as.numeric(numStem.1))) +
  stat_summary(geom = "col",  fun = "mean",fill = "deeppink", color = "black") +
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))})+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Site (sorted by AMT)") +
  scale_y_continuous(name = "Number of Stems Counted at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5))

print(p1)

p3 <- ggplot(data = phy_climate_data, aes(x = reorder(Site, wc2.1_30s_bio_1), y = as.numeric(stemLength.1))) +
  stat_summary(geom = "col",  fun = "mean",fill = "deeppink", color = "black") +
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))})+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Site (sorted by AMT)") +
  scale_y_continuous(name = "Number of Stems Counted at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5))

print(p3)

ggsave("./figures/phy_site_stmlong_AMT_bio1.png", width = 10, height = 8, dpi = 600)

p4 <- ggplot(data = phy_climate_data, aes(x = reorder(Site, wc2.1_30s_bio_1), y = as.numeric(leafArea.10))) +
  stat_summary(geom = "col",  fun = "mean",fill = "deeppink", color = "black") +
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))})+
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Site (sorted by AMT)") +
  scale_y_continuous(name = "Leaf Area (mm^2) at Week 10") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5))

print(p4)

ggsave("./figures/phy_site_leafarea_AMT_bio1.png", width = 10, height = 8, dpi = 600)

p5 <- ggplot(data = phy_climate_data, aes(x = reorder(MaternalLine, wc2.1_30s_bio_1), y = as.numeric(leafArea.10))) +
  stat_summary(geom = "col",  fun = "mean",fill = "deeppink", color = "black") +
  stat_summary(geom = "errorbar", fun.min =  "mean", fun.max = function(x){mean(x) + (sd(x)/sqrt(length(x)))})+
  scale_x_discrete(name = "Maternal Line") +
  scale_y_continuous(name = "Leaf Area (mm^2) at Week 10") +
  facet_wrap(.~Site) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5))

print(p5)

ggsave("./figures/phy_site_leafarea_AMT_bio1_facet.png", width = 10, height = 8, dpi = 600)

p2 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = as.factor(wc2.1_30s_bio_7), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Temperature Annual Range") +
  scale_y_continuous(name = "Leaf Area") +
  facet_wrap(.~Site, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p2)
 
ggsave("./figures/phy_figure_bio7.png", width = 10, height = 8, dpi = 600)

p3 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = as.factor(wc2.1_30s_bio_12), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Precipatation") +
  scale_y_continuous(name = "Leaf Area") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p3)

ggsave("./figures/phy_figure_bio12.png", width = 10, height = 8, dpi = 600)

p4 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = as.factor(wc2.1_30s_elev), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Leaf Area") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p4)

ggsave("./figures/phy_figure_elev.png", width = 10, height = 8, dpi = 600)

p5 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = as.factor(wc2.1_30s_bio_1), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Leaf Long") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p5)

ggsave("./figures/phy_figure2_bio1.png", width = 10, height = 8, dpi = 600)

p6 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = as.factor(wc2.1_30s_bio_7), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Temperature Annual Range") +
  scale_y_continuous(name = "Leaf Long") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p6)

ggsave("./figures/phy_figure2_bio7.png", width = 10, height = 8, dpi = 600)

p7 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = as.factor(wc2.1_30s_bio_12), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Precipatation") +
  scale_y_continuous(name = "Leaf Long") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p7)

ggsave("./figures/phy_figure2_bio12.png", width = 10, height = 8, dpi = 600)

p8 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = as.factor(wc2.1_30s_elev), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Leaf Long") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p8)

ggsave("./figures/phy_figure2_elev.png", width = 10, height = 8, dpi = 600)

p9 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafWide"), aes(x = as.factor(wc2.1_30s_bio_1), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Leaf Width") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p9)

ggsave("./figures/phy_figure3_bio1.png", width = 10, height = 8, dpi = 600)

p10 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafWide"), aes(x = as.factor(wc2.1_30s_bio_7), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Temperature Annual Range") +
  scale_y_continuous(name = "Leaf Width") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p10)

ggsave("./figures/phy_figure3_bio7.png", width = 10, height = 8, dpi = 600)

p11 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafWide"), aes(x = as.factor(wc2.1_30s_bio_12), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Precipatation") +
  scale_y_continuous(name = "Leaf Width") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p11)

ggsave("./figures/phy_figure3_bio12.png", width = 10, height = 8, dpi = 600)

p12 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafWide"), aes(x = as.factor(wc2.1_30s_elev), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Leaf Width") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p12)

ggsave("./figures/phy_figure3_elev.png", width = 10, height = 8, dpi = 600)

p13 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = as.factor(wc2.1_30s_bio_1), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Stem Diameter") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p13)

ggsave("./figures/phy_figure4_bio1.png", width = 10, height = 8, dpi = 600)

p14 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = as.factor(wc2.1_30s_bio_7), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Temperature Annual Range") +
  scale_y_continuous(name = "Stem Diameter") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p14)

ggsave("./figures/phy_figure4_bio7.png", width = 10, height = 8, dpi = 600)

p15 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = as.factor(wc2.1_30s_bio_12), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Precipatation") +
  scale_y_continuous(name = "Stem Diameter") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p15)

ggsave("./figures/phy_figure4_bio12.png", width = 10, height = 8, dpi = 600)

p16 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = as.factor(wc2.1_30s_elev), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Stem Diameter") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p16)

ggsave("./figures/phy_figure4_elev.png", width = 10, height = 8, dpi = 600)

p17 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemLength"), aes(x = as.factor(wc2.1_30s_bio_1), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Stem Length") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p17)

ggsave("./figures/phy_figure5_bio1.png", width = 10, height = 8, dpi = 600)

p18 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemLength"), aes(x = as.factor(wc2.1_30s_bio_7), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Temperature Annual Range") +
  scale_y_continuous(name = "Stem Length") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p18)

ggsave("./figures/phy_figure5_bio7.png", width = 10, height = 8, dpi = 600)

p19 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemLength"), aes(x = as.factor(wc2.1_30s_bio_12), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Precipatation") +
  scale_y_continuous(name = "Stem Length") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p19)

ggsave("./figures/phy_figure5_bio12.png", width = 10, height = 8, dpi = 600)

p20 <- ggplot(data = filter(phy_tidy_data, variable_name == "stemLength"), aes(x = as.factor(wc2.1_30s_elev), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Stem Length") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p20)

ggsave("./figures/phy_figure5_elev.png", width = 10, height = 8, dpi = 600)

p21 <- ggplot(data = filter(phy_tidy_data, variable_name == "numLeaf"), aes(x = as.factor(wc2.1_30s_bio_1), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Number of Leaves") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p21)

ggsave("./figures/phy_figure6_bio1.png", width = 10, height = 8, dpi = 600)

p22 <- ggplot(data = filter(phy_tidy_data, variable_name == "numLeaf"), aes(x = as.factor(wc2.1_30s_bio_7), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Temperature Annual Range") +
  scale_y_continuous(name = "Number of Leaves") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p22)

ggsave("./figures/phy_figure6_bio7.png", width = 10, height = 8, dpi = 600)

p23 <- ggplot(data = filter(phy_tidy_data, variable_name == "numLeaf"), aes(x = as.factor(wc2.1_30s_bio_12), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Annual Precipatation") +
  scale_y_continuous(name = "Number of Leaves") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p23)

ggsave("./figures/phy_figure6_bio12.png", width = 10, height = 8, dpi = 600)

p24 <- ggplot(data = filter(phy_tidy_data, variable_name == "numLeaf"), aes(x = as.factor(wc2.1_30s_elev), y = values)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  stat_anova_test(label.x.npc = 0.2, label = "{method}, F({DFn},{DFd}) = {F}, ,p = {p.format}")+
  scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Number of Leaves") +
  facet_wrap(.~variable_name, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

print(p24)
 
ggsave("./figures/phy_figure6_elev.png", width = 10, height = 8, dpi = 600)

############################################################################################

# Make sure your dataset has no more levels of MaternalLine than colors in the palette
p2 <- ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = FlowerHead, y = values, fill = FlowerHead)) +
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Maternal Line", y = "Leaf Length (cm)") +
  theme_bw() +
  theme(legend.position = "none")

p2

ggsave("./figures/phy_maternal_length_2_maia_07082024.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Site/Accession", y = "Stem Diameter") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./figures/phy_maternal_dia.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "stemLength"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Site/Accession", y = "Stem Length") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./figures/phy_maternal_stmlong.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "numLeaf"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Site/Accession", y = "Number of Leaves") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./figures/phy_maternal_numLeaf.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Site/Accession", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./figures/phy_site_area.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafWide"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Site/Accession", y = "Leaf Width") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./figures/phy_site_wide.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) +  # Using black for the outline to ensure visibility
  geom_point(position = position_jitter(width = 0.05)) +
  labs(x = "Site/Accession", y = "Leaf Long") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("./figures/phy_site_long.png", width = 10, height = 8, dpi = 600)

#################################################################################################

# Plotting Phenotype Traits in Relation to Green Pixel Area

# Leaf Length at Week 10 by Green Pixel Area

scatterplot_data <- filter(phy_tidy_data, variable_name %in% c("leafLong", "leafArea"))%>%
  filter(Date == as.Date("2024-06-11"))%>%
  pivot_wider(names_from = variable_name, values_from = values)%>%
  rowwise()%>%
  mutate(leafLong = mean(leafLong), leafArea = mean(leafArea))%>%
  ungroup()

# Create the plot
p3 <- ggplot(data = scatterplot_data, aes(x = FlowerHead, y = leafLong)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Maternal Line", y = "Stem Diameter") +
  theme_bw() +
  theme(legend.position = "none")

p3

ggsave("./figures/phy_length_area_07082024.png", width = 10, height = 8, dpi = 600)

# Create the plot and color by the variable "MaternalLine"
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafLong.10, color = MaternalLine)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +
  labs(x = "Green Pixel Area", y = "Leaf Length at Week 10 (leafLong.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafLong.10)), ceiling(max(filtered_data$leafLong.10)), by = 1))

ggsave("./figures/phy_length_area_maternal_07082024.png", width = 10, height = 8, dpi = 600)

# Leaf Width at Week 10 by Green Pixel Area

# Plot
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafWide.10)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Green Pixel Area", y = "Leaf Width at Week 10 (leafWide.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafWide.10)), ceiling(max(filtered_data$leafWide.10)), by = 1))

ggsave("./figures/phy_wide_area_07082024.png", width = 10, height = 8, dpi = 600)

# Plot and color by the variable "MaternalLine"
ggplot(data = filtered_data, aes(x = leafArea.10, y = leafWide.10, color = MaternalLine)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +
  labs(x = "Green Pixel Area", y = "Leaf Wide at Week 10 (leafWide.10)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$leafWide.10)), ceiling(max(filtered_data$leafWide.10)), by = 1))

ggsave("./figures/phy_wide_area_maternal_07082024.png", width = 10, height = 8, dpi = 600)

# Flowering by Green Pixel Area

# Plot
ggplot(data = filtered_data, aes(x = leafArea.10, y = flwrOpen.15)) +
  geom_point(color = "deeppink") +
  geom_smooth(method = "lm", color = "purple3", se = TRUE) +  # Add linear regression line
  labs(x = "Green Pixel Area", y = "Flower Open at Week 15 (flwrOpen.15)") +
  theme_bw() +
  scale_y_continuous(breaks = seq(floor(min(filtered_data$flwrOpen.15)), ceiling(max(filtered_data$flwrOpen.15)), by = 1))

ggsave("./figures/phy_flwr_area_07082024.png", width = 10, height = 8, dpi = 600)

######################################################################################

# Plotting using Facet Wrap
ggplot(data = scatterplot_data, aes(x = leafArea, y = leafLong, color = wc2.1_30s_bio_1)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +  # Add linear regression line
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95) +
  labs(x = "Green Pixel Area", y = "Leaf Long") +
  theme_bw() +
  facet_wrap(~ wc2.1_30s_bio_1)  # Replace 'maternal_line' with the actual column name for maternal lines

ggsave("./figures/phy_facet_maternal_07092024.png", width = 10, height = 8, dpi = 600)

ggplot(data = scatterplot_data, aes(x = leafArea, y = leafLong, color = wc2.1_30s_bio_7)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +  # Add linear regression line
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95) +
  labs(x = "Green Pixel Area", y = "Leaf Long") +
  theme_bw() +
  facet_wrap(~ wc2.1_30s_bio_7)  # Replace 'maternal_line' with the actual column name for maternal lines

ggsave("./figures/phy_facet_maternal1_07092024.png", width = 10, height = 8, dpi = 600)

ggplot(data = scatterplot_data, aes(x = leafArea, y = leafLong, color = wc2.1_30s_bio_12)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +  # Add linear regression line
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95) +
  labs(x = "Green Pixel Area", y = "Leaf Long") +
  theme_bw() +
  facet_wrap(~ wc2.1_30s_bio_12)  # Replace 'maternal_line' with the actual column name for maternal lines

ggsave("./figures/phy_facet_maternal2_07092024.png", width = 10, height = 8, dpi = 600)

ggplot(data = phy_tidy_data, aes(x = FlowerHead, y = leafLong)) +
  geom_point() +
  geom_smooth(method = "lm", color = "deeppink", se = TRUE) +  # Add linear regression line
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95) +
  labs(x = "Green Pixel Area", y = "Leaf Long") +
  theme_bw() +
  facet_wrap(~ wc2.1_30s_elev)  # Replace 'maternal_line' with the actual column name for maternal lines

ggsave("./figures/phy_facet_maternal3_07092024.png", width = 10, height = 8, dpi = 600)

######################################################################################################################################

# Transform "flwrOpen.2" to numeric

filter_data_fix <- filtered_data %>%
  mutate(flwrBud.2 = case_when(flwrBud.2 == "Y" ~ 1,
                               flwrBud.2 == "N" ~ 0,
                               TRUE ~ as.numeric(NA)))

unique(phy_tidy_data$variable_name)
unique(phy_tidy_data$FlowerHead)

################################################################################################################################

# Geographic Analysis

################################################################################################################################################

library(ggplot2)
library(dplyr)

# Calculate the mean leaf area for each MaternalLine
means <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  group_by(MaternalLine) %>%
  summarize(mean_value = mean(values))

# Reorder the MaternalLine factor based on the calculated means
phy_tidy_data <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  mutate(MaternalLine = factor(MaternalLine, levels = means$MaternalLine[order(means$mean_value)]))

# Create the violin plot
p1 <- ggplot(data = phy_tidy_data, aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_violin(color = "black", alpha = 0.4) + # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")")) +
  labs(x = "Maternal Line", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

p1

library(ggplot2)
library(dplyr)

# Calculate the mean leaf area for each MaternalLine
means <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  group_by(MaternalLine) %>%
  summarize(mean_value = mean(values))

# Reorder the MaternalLine factor based on the calculated means
phy_tidy_data <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  mutate(MaternalLine = factor(MaternalLine, levels = means$MaternalLine[order(means$mean_value)]))

# Create the box plot
p1 <- ggplot(data = phy_tidy_data, aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_boxplot(color = "black", alpha = 0.4) + # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")")) +
  labs(x = "Maternal Line", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

p1

library(ggplot2)
library(dplyr)

# Calculate the mean leaf area for each MaternalLine
means <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  group_by(MaternalLine) %>%
  summarize(mean_value = mean(values))

# Reorder the MaternalLine factor based on the calculated means
ordered_levels <- means %>%
  arrange(mean_value) %>%
  pull(MaternalLine)

phy_tidy_data <- phy_tidy_data %>%
  filter(variable_name == "leafArea") %>%
  mutate(MaternalLine = factor(MaternalLine, levels = ordered_levels))

# Create the box plot
p1 <- ggplot(data = phy_tidy_data, aes(x = MaternalLine, y = values, fill = MaternalLine)) +
  geom_boxplot(color = "black", alpha = 0.4) + # Using black for the outline to ensure visibility
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.25, width = 0.2) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(name = expression("Leaf area ("~mm^2~")")) +
  labs(x = "Maternal Line", y = "Leaf Area") +
  theme_bw() +
  theme(legend.position = "none")

p1

# Make graph with accession as x and traits as y

# Find climate variables with significance (rainfall, elevation, temperature, solar radiation)

#####################################################################################################

# ANOVA

install.packages("esquisse")
library(esquisse)
esquisser(phy_data)

phy_data <- read_csv("data/physaria_buf_climate_data_20240717.csv")

# list of traits: Leaf Area, Leaf Length, Leaf Width

ggplot(data = phy_data, aes(x = wc2.1_30s_bio_16, y = leafLong.10)) +
  geom_point(color = "deeppink") +
  theme_bw() 

ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = wc2.1_30s_bio_1, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Mean Temperature")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_area_regression_bio1.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = wc2.1_30s_bio_7, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Temperature Range")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_area_regression_bio7.png", width = 10, height = 8, dpi = 600)

install.packages("lme4")


install.packages("lmerTest")
library(lmerTest)

anova1 <- lm(leafArea.10 ~ paste0(Site, as.character(MaternalLine)), data = phy_climate_data)

summary(anova1)
anova(anova1)

phy_climate_data_uni <- phy_climate_data %>%
  mutate(uni_MaternalLine = paste0(Site, as.character(MaternalLine)))

anova2 <- lm(stemLength.1 ~ MaternalLine, data = phy_climate_data)

summary(anova2)
anova(anova2)

phy_climate_data$leafLong.10 <- as.numeric(phy_climate_data$leafLong.10)
anova2 <- lmer(leafLong.10 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova2)
h_leafLong <- lmer(leafLong.10 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_leafLong)

phy_climate_data$numRos.1 <- as.numeric(phy_climate_data$numRos.1)
anova3 <- lmer(numRos.1 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova3)
h_numRos <- lmer(numRos.1 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_numRos)

phy_climate_data$numStem.1 <- as.numeric(phy_climate_data$numStem.1)
anova4 <- lmer(numStem.1 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova4)
h_numStem <- lmer(numStem.1 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_numStem)

phy_climate_data$stemDia.1 <- as.numeric(phy_climate_data$stemDia.1)
anova5 <- lmer(stemDia.1 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova5)
h_stemDia <- lmer(stemDia.1 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_stemDia)

phy_climate_data$stemLength.1 <- as.numeric(phy_climate_data$stemLength.1)
anova6 <- lmer(stemLength.1 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova6)
h_stemLength <- lmer(stemLength.1 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_stemLength)

phy_climate_data$numLeaf.1 <- as.numeric(phy_climate_data$numLeaf.1)
anova7 <- lmer(numLeaf.1 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova7)
h_numLeaf <- lmer(numLeaf.1 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_numLeaf)

phy_climate_data$leafWide.10 <- as.numeric(phy_climate_data$leafWide.10)
anova8 <- lmer(leafWide.10 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)
anova(anova8)
h_leafWide <- lmer(leafWide.10 ~ (1|Site/MaternalLine), data = phy_climate_data)
summary(h_leafWide)

lm2 <- lmer(leafLong.10 ~ wc2.1_30s_bio_1 + (1|MaternalLine/FlowerHead), data = phy_climate_data)

summary(lm1)

anova(lm1)

ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = FlowerHead, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Maternal Line")+
  scale_y_continuous(name = "Leaf Area")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_area_regression_bio12.png", width = 5, height = 4, dpi = 1200)

ggplot(data = filter(phy_climate_data, variable_name == "leafArea"), aes(x = wc2.1_30s_elev, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Elevation")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_area_regression_elev.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = wc2.1_30s_bio_1, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "stemDia"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "stemDia"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "stemDia"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "stemDia"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Mean Temperature")+
  scale_y_continuous(name = "Stem Diameter")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_dia_regression_bio1.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = wc2.1_30s_bio_7, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "stemDia"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "stemDia"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "stemDia"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "stemDia"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Temperature Range")+
  scale_y_continuous(name = "Stem Diameter")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_dia_regression_bio7.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = wc2.1_30s_bio_12, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "stemDia"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "stemDia"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "stemDia"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "stemDia"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Precipatation")+
  scale_y_continuous(name = "Stem Diameter")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_dia_regression_bio12.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = wc2.1_30s_elev, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "stemDia"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "stemDia"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "stemDia"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "stemDia"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Elevation")+
  scale_y_continuous(name = "Stem Diameter")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_dia_regression_elev.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = wc2.1_30s_bio_1, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Mean Temperature")+
  scale_y_continuous(name = "Leaf Length")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_long_regression_bio1.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = wc2.1_30s_bio_7, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Annual Temperature Range")+
  scale_y_continuous(name = "Leaf Length")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_long_regression_bio7.png", width = 10, height = 8, dpi = 600)

ggplot(data = filter(phy_tidy_data, variable_name == "leafLong"), aes(x = FlowerHead, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafLong"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafLong"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafLong"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafLong"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Maternal Line")+
  scale_y_continuous(name = "Leaf Length")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = filter(phy_tidy_data, variable_name == "leafArea"), aes(x = FlowerHead, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "leafArea"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "leafArea"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "leafArea"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "leafArea"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Maternal Line")+
  scale_y_continuous(name = "Leaf Area")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = filter(phy_tidy_data, variable_name == "stemDia"), aes(x = FlowerHead, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "stemDia"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "stemDia"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "stemDia"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "stemDia"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Maternal Line")+
  scale_y_continuous(name = "Stem Diameter")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = filter(phy_tidy_data, variable_name == "stemLength"), aes(x = FlowerHead, y = values))+
  geom_point()+
  geom_point(data = filter(phy_tidy_data, variable_name == "stemLength"), color = "deeppink")+
  stat_smooth(data = filter(phy_tidy_data, variable_name == "stemLength"), method = "lm", color = "blue")+
  stat_cor(data = filter(phy_tidy_data, variable_name == "stemLength"),method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(data = filter(phy_tidy_data, variable_name == "stemLength"),label.x.npc = 0, label.y.npc = 0.95)+
  scale_x_continuous(name = "Maternal Line")+
  scale_y_continuous(name = "Stem Length")+
  facet_wrap(.~variable_name, nrow = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = phy_climate_data, aes(x = wc2.1_30s_bio_1, y = leafArea.10)) +
  geom_point() +
  geom_point(data = phy_climate_data, color = "deeppink") +
  stat_smooth(data = phy_climate_data, 
              method = "lm", 
              formula = y ~ FlowerHead + (1/MaternalLine:FlowerHead), 
              color = "blue") +
  stat_cor(data = phy_climate_data, 
           method = "pearson", 
           label.x.npc = 0, 
           label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, 
                        formula = y ~ FlowerHead + (1/MaternalLine:FlowerHead), 
                        label.x.npc = 0, 
                        label.y.npc = 0.95) +
  scale_x_continuous(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Leaf Length at Week 10 (06/11/2024)") +
  facet_wrap(.~wc2.1_30s_bio_1, nrow = 1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom")

# Load the viridis package
install.packages("viridis")
library(viridis)

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = leafLong.10, color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Leaf Length (cm) at Week 10 (06/11/2024)") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

# Check the data type of the 'stemDia.1' column
str(phy_climate_data)

# Convert 'stemDia.1' column to numeric if it is not already
phy_climate_data$stemDia.1 <- as.numeric(phy_climate_data$stemDia.1)

# Now, plot using ggplot2
ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(stemDia.1), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Stem Diameter (cm) at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

##################################################################################

ggplot(data = phy_climate_data, aes(x = wc2.1_30s_bio_1, y = leafArea.10)) +
  geom_point(color = "deeppink") +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Leaf Area") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

lm_phy_1 <- lmer(leafArea.10 ~ wc2.1_30s_bio_1 + (1|Site/MaternalLine), data = phy_climate_data)

summary(lm_phy_1)
anova(lm_phy_1)

ggplot(data = phy_climate_data, aes(x = as.factor(wc2.1_30s_bio_1), y = leafArea.10)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Leaf Area (mm^2)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

anova1_phy <- lmer(leafArea.10 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)

summary(anova1_phy)
anova(anova1_phy)

ggplot(data = phy_climate_data, aes(x = wc2.1_30s_bio_1, y = as.numeric(numRos.1))) +
  geom_point(color = "deeppink") +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Number of Rosettes at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

phy_climate_data$numRos.1 <- as.numeric(phy_climate_data$numRos.1)

lm_phy_2 <- lmer(numRos.1 ~ wc2.1_30s_bio_1 + (1|Site/MaternalLine), data = phy_climate_data)

summary(lm_phy_2)
anova(lm_phy_2)

ggplot(data = phy_climate_data, aes(x = as.factor(wc2.1_30s_bio_1), y = numRos.1)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Number of Rosettes at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

anova2_phy <- lmer(numRos.1 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)

summary(anova2_phy)
anova(anova2_phy)

ggplot(data = phy_climate_data, aes(x = wc2.1_30s_bio_1, y = as.numeric(stemLength.1))) +
  geom_point(color = "deeppink") +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Stem Length (cm) measured at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_stemLong_regression_bio1.png", width = 10, height = 8, dpi = 600)

phy_climate_data$leafWide.10 <- as.numeric(phy_climate_data$leafWide.10)

lm_phy_3 <- lmer(leafWide.10 ~ wc2.1_30s_bio_1 + (1|Site/MaternalLine), data = phy_climate_data)

summary(lm_phy_3)
anova(lm_phy_3)

ggplot(data = phy_climate_data, aes(x = as.factor(wc2.1_30s_bio_1), y = leafWide.10)) +
  geom_boxplot(fill = "deeppink", color = "black") +
  scale_x_discrete(name = "Annual Mean Temperature") +
  scale_y_continuous(name = "Number of Rosettes at Week 1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

anova3_phy <- lmer(leafWide.10 ~ Site + (1|Site:MaternalLine), data = phy_climate_data)

summary(anova3_phy)
anova(anova3_phy)

################################################################################################

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(numRos.1), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Number of Rosettes at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(numStem.1), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Number of Stems at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(stemLength.1), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Stem Lengths at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_stmlong_regression_temp.png", width = 10, height = 8, dpi = 600)

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(numLeaf.1), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Number of Leaves at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(leafLong.10), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Leaf Length (cm) at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = phy_climate_data, aes(x = FlowerHead, y = as.numeric(leafWide.10), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Leaf Width (cm) at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("./figures/phy_leafwide_regression_temp.png", width = 10, height = 8, dpi = 600)

ggplot(data = phy_climate_data, aes(x = MaternalLine, y = as.numeric(leafWide.10), color = wc2.1_30s_bio_1)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Leaf Width (cm) at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggplot(data = phy_climate_data, aes(x = Site, y = as.numeric(stemLength.1),)) +
  geom_point() +
  stat_smooth(data = phy_climate_data, method = "lm", color = "blue") +
  stat_cor(data = phy_climate_data, method = "pearson", label.x.npc = 0, label.y.npc = 0.80) +
  stat_regline_equation(data = phy_climate_data, label.x.npc = 0, label.y.npc = 0.95) +
  scale_x_continuous(name = "Maternal Line") +
  scale_y_continuous(name = "Leaf Width (cm) at Week 1") +
  scale_color_viridis_c(name = "Annual Mean Temperature", option = "plasma") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")