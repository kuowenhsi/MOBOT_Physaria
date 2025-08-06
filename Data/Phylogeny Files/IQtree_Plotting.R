

###########################################################################
################################ Phylogeny ################################
###########################################################################

library(ggtree)
library(treeio)
library(tidyverse)

setwd("~/Desktop/Physaria_Data/close_outgroup/quick phylogeny")

#### 1) Load the tree ####
tree <- read.tree("Physaria_Chr_1_1-28212464.filtered_subsetfilter_filtered_all_samples.min4.phy.treefile")
class(tree)

#### 2) Rectangular plot ####
r <- ggtree(tree, layout = "rectangular") + 
  geom_tiplab(size = 2) +
  theme_tree()
ggsave("iqtree_phylogeny_plot_rectangular.pdf", plot = r, width = 10, height = 8)

#### 2) Circular plot ####
c <- ggtree(tree, layout = "circular") + 
  geom_tiplab(size = 2) +
  theme_tree()
ggsave("iqtree_phylogeny_plot_circular.pdf", plot = c, width = 10, height = 8)

#### 3) Slanted plot ####
s <- ggtree(tree, layout = "slanted") + 
  geom_tiplab(size = 2) +
  theme_tree()
ggsave("iqtree_phylogeny_plot_slanted.pdf", plot = s, width = 10, height = 8)
