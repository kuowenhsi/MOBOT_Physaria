

####################################################################################
##################################### PCadapt ######################################
####################################################################################

library(pcadapt)
library(ggplot2)
library(tidyverse)
library(data.table)
library(qvalue)
library(cowplot)

setwd("~/Desktop/Physaria_Data/pcadapt/PCAadapt")

#######################################################################
# Change K value as needed

# Create SNP Summary file, change K value as needed
# 1_1
pcadapt_data1_1 <- read.pcadapt("Physaria_Chr_1_1_ldpruned.bed", type = "bed")
pcadapt_result1_1 <- pcadapt(pcadapt_data1_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info1_1<- read_tsv("Physaria_Chr_1_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result1_1$pvalues)

# 1_2
pcadapt_data1_2 <- read.pcadapt("Physaria_Chr_1_2_ldpruned.bed", type = "bed")
pcadapt_result1_2 <- pcadapt(pcadapt_data1_2, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info1_2 <- read_tsv("Physaria_Chr_1_2_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result1_2$pvalues)

# 2_1
pcadapt_data2_1 <- read.pcadapt("Physaria_Chr_2_1_ldpruned.bed", type = "bed")
pcadapt_result2_1 <- pcadapt(pcadapt_data2_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info2_1 <- read_tsv("Physaria_Chr_2_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result2_1$pvalues)

# 2_2
pcadapt_data2_2 <- read.pcadapt("Physaria_Chr_2_2_ldpruned.bed", type = "bed")
pcadapt_result2_2 <- pcadapt(pcadapt_data2_2, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info2_2 <- read_tsv("Physaria_Chr_2_2_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result2_2$pvalues)

# 3_1
pcadapt_data3_1 <- read.pcadapt("Physaria_Chr_3_1_ldpruned.bed", type = "bed")
pcadapt_result3_1 <- pcadapt(pcadapt_data3_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info3_1 <- read_tsv("Physaria_Chr_3_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result3_1$pvalues)

# 3_3
pcadapt_data3_3 <- read.pcadapt("Physaria_Chr_3_3_ldpruned.bed", type = "bed")
pcadapt_result3_3 <- pcadapt(pcadapt_data3_3, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info3_3 <- read_tsv("Physaria_Chr_3_3_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result3_3$pvalues)

# 4_1
pcadapt_data4_1 <- read.pcadapt("Physaria_Chr_4_1_ldpruned.bed", type = "bed")
pcadapt_result4_1 <- pcadapt(pcadapt_data4_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info4_1 <- read_tsv("Physaria_Chr_4_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result4_1$pvalues)

# 4_3
pcadapt_data4_3 <- read.pcadapt("Physaria_Chr_4_3_ldpruned.bed", type = "bed")
pcadapt_result4_3 <- pcadapt(pcadapt_data4_3, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info4_3 <- read_tsv("Physaria_Chr_4_3_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result4_3$pvalues)

# 5_1
pcadapt_data5_1 <- read.pcadapt("Physaria_Chr_5_1_ldpruned.bed", type = "bed")
pcadapt_result5_1 <- pcadapt(pcadapt_data5_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info5_1 <- read_tsv("Physaria_Chr_5_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result5_1$pvalues)

# 5_3
pcadapt_data5_3 <- read.pcadapt("Physaria_Chr_5_3_ldpruned.bed", type = "bed")
pcadapt_result5_3 <- pcadapt(pcadapt_data5_3, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info5_3 <- read_tsv("Physaria_Chr_5_3_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result5_3$pvalues)

# 6_1
pcadapt_data6_1 <- read.pcadapt("Physaria_Chr_6_1_ldpruned.bed", type = "bed")
pcadapt_result6_1 <- pcadapt(pcadapt_data6_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info6_1 <- read_tsv("Physaria_Chr_6_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result6_1$pvalues)

# 6_2
pcadapt_data6_2 <- read.pcadapt("Physaria_Chr_6_2_ldpruned.bed", type = "bed")
pcadapt_result6_2 <- pcadapt(pcadapt_data6_2, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info6_2 <- read_tsv("Physaria_Chr_6_2_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result6_2$pvalues)

# 7_1
pcadapt_data7_1 <- read.pcadapt("Physaria_Chr_7_1_ldpruned.bed", type = "bed")
pcadapt_result7_1 <- pcadapt(pcadapt_data7_1, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info7_1 <- read_tsv("Physaria_Chr_7_1_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result7_1$pvalues)

# 7_3
pcadapt_data7_3 <- read.pcadapt("Physaria_Chr_7_3_ldpruned.bed", type = "bed")
pcadapt_result7_3 <- pcadapt(pcadapt_data7_3, K = 7, min.maf = 0.25, LD.clumping = list(size = 5000, thr = 0.2))
SNP_info7_3 <- read_tsv("Physaria_Chr_7_3_ldpruned.bim", col_names = c("CHR", "INFO", "FILTER", "POSITION", "REF", "ALT"))%>%
  mutate(p_value = pcadapt_result7_3$pvalues)

combined_snp_df <- bind_rows(
  SNP_info1_1, SNP_info1_2,
  SNP_info2_1, SNP_info2_2,
  SNP_info3_1, SNP_info3_3,
  SNP_info4_1, SNP_info4_3,
  SNP_info5_1, SNP_info5_3,
  SNP_info6_1, SNP_info6_2,
  SNP_info7_1, SNP_info7_3
)

readr::write_tsv(combined_snp_df, "combined_SNP_info1.txt")

#######################################################################

# Load chromosome information from a .fasta index file
chr_len_temp <- read_tsv("Physaria_hap1_1.0.fasta.fai", col_names = c("chr", "end")) %>%
  select(1:2) %>%
  mutate(start = 1) %>%
  filter(str_detect(chr, "Chr_")) %>%
  mutate(chr = as.integer(str_remove(chr, "Chr_"))) %>%
  arrange(chr) %>%
  mutate(lag_pos = lag(end, default = 0)) %>%
  mutate(pos_pad = cumsum(lag_pos)) %>%
  mutate(padded_start = start + pos_pad - 1, padded_end = end + pos_pad) %>%
  mutate(padded_chr_pos = (padded_start + padded_end) / 2)

# Load and process PCadapt data
PCadapt_data <- read_tsv("combined_SNP_info.txt") %>%
  left_join(chr_len_temp, by = c("CHR" = "chr")) %>%
  mutate(padded_pos = POSITION + pos_pad, qvalue = qvalue(p_value)$qvalues) %>%
  mutate(dot_color = case_when(p_value < 5e-8 ~ "black", TRUE ~ "gray80")) %>%
  mutate(name = "PCadapt")

PCadapt_data <- PCadapt_data %>%
  mutate(p_value = as.numeric(p_value)) %>%
  filter(!is.na(p_value))

# Q-Q plot for PCadapt result
p_QQ_PCadapt <- ggplot(data = PCadapt_data, aes(sample = -log(p_value))) +
  stat_qq(aes(x = after_stat(theoretical) / log(10), y = after_stat(sample) / log(10)), distribution = qexp, size = 0.5) +
  stat_qq_line(aes(x = after_stat(x) / log(10), y = after_stat(y) / log(10)), distribution = qexp) +
  geom_hline(yintercept = -log10(5e-8), color = "red") +
  xlab(expression("Theoretical" ~ "-" * log[10] * "(p)")) +
  ylab(expression("Observed" ~ "-" * log[10] * "(p)")) +
  labs(title = "Q-Q plot for the PCadapt result") +
  theme_bw() +
  theme(panel.grid = element_blank())

p_QQ_PCadapt
ggsave(filename = "PCadapt_QQplot.png", plot = p_QQ_PCadapt, width = 6, height = 5, dpi = 300)

#######################################################################

pcap <- ggplot(data = PCadapt_data, aes(x = padded_pos, y = -log10(p_value))) +
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:16)), inherit.aes = FALSE) +
  geom_point(color = PCadapt_data$dot_color, size = 0.5) +
  geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous(
    name = "",
    expand = c(0, 0),
    breaks = chr_len_temp$padded_chr_pos,
    labels = chr_len_temp$chr
  ) +
  scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")

pcap

ggsave(filename = "PCadapt.png", plot = pcap, width = 6, height = 5, dpi = 300)

