
cd /Users/meganconnorlaptop/Desktop/Physaria_VCF_secondfilter/VCF\ FIles

conda activate vcftools-env

vcftools --gzvcf Physaria_Chr_1_1-28212464.filtered.vcf.gz --het --out Physaria_Chr_1_1-output