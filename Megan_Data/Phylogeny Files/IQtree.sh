
#!/bin/bash

######################################################################################################################

# Step 1:
curl -O https://raw.githubusercontent.com/edgardomortiz/vcf2phylip/master/vcf2phylip.py
chmod +x vcf2phylip.py

# Step 2:
python vcf2phylip.py -i Physaria_Chr_1_1-28212464.filtered_subsetfilter_filtered_all_samples.vcf.gz

# Step 3: without bootstrapping and SH-aLRT
# faster (5-20min)
iqtree -s Physaria_Chr_1_1-28212464.filtered_subsetfilter_filtered_all_samples.min4.phy -m GTR+G -fast -nt AUTO 

# less (30-90min)
iqtree -s Physaria_Chr_1_1-28212464.filtered_subsetfilter_filtered_all_samples.min4.phy -m GTR+G -nt AUTO 

