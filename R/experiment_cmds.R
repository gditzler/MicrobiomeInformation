# experiment with different level of multi-dimentsional scaling 
library("ggplot2")
library("distrEx")
  
document()

# set up program constants
col_name = "DIET_TYPE"   # column in the map files for the labels
lvl = 0.75                # filter level for OTUs
nbins = 50               # number of bins for estimating the pdfs

# set the paths of the biom & map files then load them 
biom_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet.biom"
map_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet-OV.txt"
d_name <- "ag_gut_diet_ov"

# load the biom & map files
biom_df <- load_biom_matrix(biom_fp)
map_df <- load_metadata(map_fp)

# scale the matrix 
data <- scale_matrix(biom_df, rescale=FALSE)
biom_df$data_dense <- data
labels <- retrieve_labels(map_df, biom_df$sample_ids, col_name)

# filter the otus to remove the low ranking otus that are very low in terms of 
# the abunance
lst <- filter_otus(biom_df, lvl=lvl)
data_filter <- lst$data

d <- 

save.image("data/experiment_cmds.RData")
