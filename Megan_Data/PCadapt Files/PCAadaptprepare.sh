

cd /Users/meganconnorlaptop/Desktop/close_outgroup/PCAadapt
conda activate plink-env


################################################# Running admixture from scratch ###################################################
################# Step 1: Convert VCF --> Plink ##############################
plink2 --vcf Physaria_Chr_7_3.vcf.gz \
  --make-bed \
  --out Physaria_Chr_7_3 \
  --double-id \
  --allow-extra-chr

# output = Physaria_Chr_1.{bed,bim,fam}

################# Step 2: Remove data #############
plink2 \
  --bfile Physaria_Chr_7_3 \
  --remove remove_samples.txt \
  --make-bed \
  --out Physaria_Chr_7_3_excluded \
  --allow-extra-chr

# output = Physaria_Chr_1_excluded.{bed,bim,fam}

################# Step 3: Simple imputation of missing genotypes #############
plink --bfile Physaria_Chr_7_3_excluded \
  --make-bed \
  --out Physaria_Chr_7_3_imputed \
  --fill-missing-a2 \
  --allow-extra-chr

# output = Physaria_Chr_1_imputed.{bed,bim,fam}

################### Step 4: Fix chromosome names ##############
awk '{gsub(/^Chr_/, "", $1); print}' OFS='\t' Physaria_Chr_7_3_imputed.bim > Physaria_Chr_7_3_fixed.bim
cp Physaria_Chr_7_3_imputed.bed Physaria_Chr_7_3_fixed.bed
cp Physaria_Chr_7_3_imputed.fam Physaria_Chr_7_3_fixed.fam

# output = Physaria_Chr_1_fixed.{bed,bim,fam}

################### Step 5: LD prune #######################################
plink --bfile Physaria_Chr_7_3_fixed \
  --indep-pairwise 50 5 0.2 \
  --out Physaria_Chr_7_3_pruned

# output = Physaria_Chr_1_pruned.prune.{in,out}

plink --bfile Physaria_Chr_7_3_fixed \
  --extract Physaria_Chr_7_3_pruned.prune.in \
  --make-bed \
  --out Physaria_Chr_7_3_ldpruned

# output = Physaria_Chr_1_ldpruned.{bed,bim,fam}


