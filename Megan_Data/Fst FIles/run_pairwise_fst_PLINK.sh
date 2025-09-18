
#How to run the code
#cd /Users/meganconnorlaptop/Desktop/Physaria_VCF_secondfilter/Fst_Stuff
#conda activate plink-env

#!/bin/bash
#PLINK

# Step 1: Convert VCF > PLINK, Exclude Data, Simple Imputation
plink2 --vcf Physaria_Chr_1_1-28212464.filtered.vcf.gz \
  --make-bed \
  --out Physaria_Chr_1 \
  --double-id \
  --allow-extra-chr

plink --bfile Physaria_Chr_1 \
  --make-bed \
  --out Physaria_Chr_1_imputed \
  --fill-missing-a2 \
  --allow-extra-chr

plink2 --bfile Physaria_Chr_1_imputed \
  --remove remove_samples_fixed.txt \
  --make-pgen \
  --out physaria_clean \
  --allow-extra-chr

# Step 3: Create a population cluster file (in R) and fix header for .psam
library(readr)
library(dplyr)
library(stringr)

meta <- read_csv("Physaria_MergedData_20240703.csv")

meta <- meta %>%
  mutate(
    FID = paste0("Physaria_", str_pad(Index, width = 3, pad = "0")),
    IID = FID,
    POP = as.character(MaternalLine)
  ) %>%
  select(FID, IID, POP)

# Write the file with no header (or add header if you prefer)
write.table(meta, "physaria.pop", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Read the old .psam file (assuming no header or a broken one)
psam <- read.table("physaria_clean.psam", header = FALSE)

# Assign correct column names (assuming only FID and IID are present)
colnames(psam) <- c("FID", "IID")

# Write new .psam file with correct PLINK 2 header format
write.table(psam, "physaria_clean.psam", quote = FALSE, row.names = FALSE, col.names = TRUE)

# Step 5: Fix header
sed -i '' '1s/^/#/' physaria_clean.psam

# Step 4: Run Fst
plink2 --pfile physaria_clean \
  --fst POP \
  --out physaria_fst \
  --allow-extra-chr

