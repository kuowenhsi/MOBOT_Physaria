
#!/bin/bash

################################################# Running admixture from scratch ###################################################
################# Step 1: Convert VCF --> Plink ##############################
plink2 --vcf Physaria_Chr_1_1-28212464.filtered.vcf.gz \
  --make-bed \
  --out Physaria_Chr_1 \
  --double-id \
  --allow-extra-chr

# output = Physaria_Chr_1.{bed,bim,fam}

################# Step 2: Exclude data #############
plink2 \
  --bfile Physaria_Chr_1 \
  --remove remove_samples.txt \
  --make-bed \
  --out Physaria_Chr_1_excluded \
  --allow-extra-chr

## PCA using excluded file
plink --bfile Physaria_Chr_1_excluded --pca 178 --out Physaria_Chr_1_removed --allow-extra-chr

# output = Physaria_Chr_1_excluded.{bed,bim,fam}

################# Step 3: Simple imputation of missing genotypes #############
plink --bfile Physaria_Chr_1_excluded \
  --make-bed \
  --out Physaria_Chr_1_imputed \
  --fill-missing-a2 \
  --allow-extra-chr

# output = Physaria_Chr_1_imputed.{bed,bim,fam}

################### Step 4: Fix chromosome names ##############
awk '{gsub(/^Chr_/, "", $1); print}' OFS='\t' Physaria_Chr_1_imputed.bim > Physaria_Chr_1_fixed.bim
cp Physaria_Chr_1_imputed.bed Physaria_Chr_1_fixed.bed
cp Physaria_Chr_1_imputed.fam Physaria_Chr_1_fixed.fam

# output = Physaria_Chr_1_fixed.{bed,bim,fam}

################### Step 5: LD prune #######################################
plink --bfile Physaria_Chr_1_fixed \
  --indep-pairwise 50 5 0.2 \
  --out Physaria_Chr_1_pruned

# output = Physaria_Chr_1_pruned.prune.{in,out}

plink --bfile Physaria_Chr_1_fixed \
  --extract Physaria_Chr_1_pruned.prune.in \
  --make-bed \
  --out Physaria_Chr_1_ldpruned

# output = Physaria_Chr_1_ldpruned.{bed,bim,fam}

##################### Step 6: Run admixture ################################
admixture --cv=10 Physaria_Chr_1_ldpruned.bed 4
##change cv to number of iterations you would like to do

# output = Physaria_Chr_1_ldpruned.4.Q and Physaria_Chr_1_ldpruned.4.P

################################################# Best K value ##############################################################

for K in {1..20}; do
  echo "K=${K}" >> cv_errors_summary.txt
  grep "CV error" Physaria_Chr_1_ldpruned_K${K}_cv10.log >> cv_errors_summary.txt
done

######################################################### Running PCA ########################################################
# This will be done on the Physaria_Chr_1_ldpruned.{bed,.bim,.fam} files 
# since simple imputation, data removal, and pruning has been done

plink --bfile Physaria_Chr_1_ldpruned --pca --out Physaria_Chr_1_pca


