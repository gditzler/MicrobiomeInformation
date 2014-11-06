# build the documentation and load the package into the environment
library("devtools")
library("ggplot2")
library("gplots")
library("reshape2")
library("plyr")
library("Distance")

document()

# set up program constants
col_name = "DIET_TYPE"
lvl = 0.5
nbins = 50

# set the paths of the biom & map files then load them 
biom_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet.biom"
map_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet-OV.txt"
biom_df <- load_biom_matrix(biom_fp)
map_df <- load_metadata(map_fp)

labels <- retrieve_labels(map_df, biom_df$sample_ids, col_name)
lst <- filter_otus(biom_df, lvl=lvl)
data <- lst$data

mi <- mi_matrix(data, discrete = TRUE, disc = "equalwidth", nbins = nbins, method = "emp")
cmi <- cmi_matrix(data, labels, discrete = TRUE, disc = "equalwidth", nbins = nbins, method = "emp")


image.plot(1:length(mi[1,]), 1:length(mi[,1]), log(mi), xlab="", ylab="")
image.plot(1:length(mi[1,]), 1:length(mi[,1]), log(cmi), xlab="", ylab="")

heatmap(1+data, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'canberra'))




save.image("data/demo.RData")
