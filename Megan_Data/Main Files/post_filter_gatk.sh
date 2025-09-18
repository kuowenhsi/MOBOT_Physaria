#!/bin/bash

# Usage: bash post_filter_gatk.sh input.vcf.gz OUTPUT_DIR output_prefix
#  bash post_filter_gatk.sh Physaria_Chr_1_1-28212464.filtered.vcf.gz cleaned_vcfs Physaria

IN_VCF=$1         # e.g. input.vcf.gz
OUTPUT_DIR=$2     # e.g. cleaned_vcfs
OUT_PREFIX="${IN_VCF%.filtered.vcf.gz}"     # e.g. Boltonia_filtered

cd /Users/meganconnorlaptop/Desktop/Physaria_VCF_secondfilter
set -e


# Ensure the output directory exists (relative to current working dir)
mkdir -p "../${OUTPUT_DIR}"
TMP_DIR="./tmp_${OUT_PREFIX}"
mkdir -p "$TMP_DIR"

echo "Step 1: Keep only biallelic sites"
bcftools view -m2 -M2 "$IN_VCF" -Oz -o "$TMP_DIR/${OUT_PREFIX}.biallelic.vcf.gz"

echo "Step 2: Mask genotypes with GQ<10 OR DP<3 OR DP>50 as missing"
bcftools +setGT "$TMP_DIR/${OUT_PREFIX}.biallelic.vcf.gz" -- -t q -n . \
    -i 'FMT/DP<3 | FMT/GQ<10 | FMT/DP>50' > "$TMP_DIR/${OUT_PREFIX}.masked.vcf.gz"

# Tag	Name	Meaning
# AN	Allele Number	Total number of alleles in called genotypes at this site (e.g., 2 × number of diploid samples with non-missing genotypes)
# AC	Allele Count	Number of alternate alleles observed (across all samples with valid calls)
# AF	Allele Frequency	Frequency of alternate allele(s), calculated as AC / AN
# NS	Number of Samples	Number of samples that have a non-missing genotype (GT ≠ ./.) at this site

echo "Step 3: Fill INFO tags: AN, AC, AF, NS"
bcftools +fill-tags "$TMP_DIR/${OUT_PREFIX}.masked.vcf.gz" > "$TMP_DIR/${OUT_PREFIX}.tags.vcf.gz" -- -t AN,AC,AF,NS

echo "Step 4: Filter variants with MAF ≥ 0.01 and missing rate < 10%"
bcftools view -i 'INFO/AF >= 0.01' "$TMP_DIR/${OUT_PREFIX}.tags.vcf.gz" | \
bcftools view -i 'COUNT(GT="mis")/N_SAMPLES < 0.10' -Oz -o "$TMP_DIR/${OUT_PREFIX}.filtered.tmp.vcf.gz"

echo "Step 5: Fix ploidy to diploid"
bcftools +fixploidy "$TMP_DIR/${OUT_PREFIX}.filtered.tmp.vcf.gz" > "$TMP_DIR/${OUT_PREFIX}.filtered.vcf"

echo "Step 6: Compress and clean up INFO"
bcftools annotate -x ^INFO/AN,INFO/AC,INFO/AF,INFO/NS "$TMP_DIR/${OUT_PREFIX}.filtered.vcf" -Oz -o "../${OUTPUT_DIR}/${OUT_PREFIX}.filtered.vcf.gz"

echo "Step 7: Index final VCF"
bcftools index "../${OUTPUT_DIR}/${OUT_PREFIX}.filtered.vcf.gz"

# Clean up
rm -r "$TMP_DIR"

echo "✅ DONE. Final output: ../${OUTPUT_DIR}/${OUT_PREFIX}.filtered.vcf.gz"
