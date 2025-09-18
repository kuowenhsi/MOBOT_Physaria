

####################################################################################
############################ CV Errors: Best K #####################################
####################################################################################

library(tidyverse)

setwd("~/Desktop/Physaria_Data/VCF-Plink-Remove-Admixture/ADMIXTURE_data_from_wen")

#### 1) Read the CV error summary file ####
lines <- readLines("cv_errors_summary.txt")

#### 2) Extract K and CV error values from each line ####
cv_df <- do.call(rbind, lapply(lines, function(line) {
  matches <- regmatches(line, regexec("CV error \\(K=(\\d+)\\): ([0-9\\.]+)", line))
  c(K = as.numeric(matches[[1]][2]), CV_error = as.numeric(matches[[1]][3]))
})) %>%
  as.data.frame() %>%
  setNames(c("K", "CV_error"))

#### 4) Plot ####
cv_plot <- ggplot(cv_df, aes(x = K, y = CV_error)) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  labs(
    title = "ADMIXTURE Cross-Validation Error by K",
    x = "Number of Ancestral Populations (K)",
    y = "Cross-Validation Error"
  ) +
  theme_bw()

cv_plot

#### 5) Save plot with ggsave ####
ggsave("admixture_cv_error_plot.png", plot = cv_plot, width = 8, height = 6, dpi = 300)
