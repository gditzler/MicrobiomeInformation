# build the documentation and load the package into the environment
library("devtools")
library("ggplot2")
library("gplots")
library("reshape2")
library("plyr")
library("fields")

document()

# set up program constants
col_name = "DIET_TYPE"   # column in the map files for the labels
lvl = 0.5                # filter level for OTUs
nbins = 7               # number of bins for estimating the pdfs

# set the paths of the biom & map files then load them 
biom_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet.biom"
map_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet-OV.txt"

# load the biom & map files
biom_df <- load_biom_matrix(biom_fp)
map_df <- load_metadata(map_fp)

# scale the matrix 
data <- scale_matrix(biom_df, rescale=FALSE)
biom_df$data_dense <- data

# extract the labels from the map file data struture
labels <- retrieve_labels(map_df, biom_df$sample_ids, col_name)

# filter the otus to remove the low ranking otus that are very low in terms of 
# the abunance
lst <- filter_otus(biom_df, lvl=lvl)
data <- lst$data

# compute mi & cmi matrices
mi <- mi_matrix(data, discrete = TRUE, disc = "equalwidth", nbins = nbins, method = "emp")
cmi <- cmi_matrix(data, labels, discrete = TRUE, disc = "equalwidth", nbins = nbins, method = "emp")

# what to plot...
image.plot(1:length(mi[1,]), 1:length(mi[,1]), log(mi), xlab="", ylab="")
image.plot(1:length(mi[1,]), 1:length(mi[,1]), log(cmi), xlab="", ylab="")

data2 <- data
data2 <- data2 / t(replicate(nrow(data2), colSums(data2)))

hm_df1 <- heatmap(1+data, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'canberra'))
hm_df2 <- heatmap(data, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'binary'))
hm_df3 <- heatmap(data, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'euclidean'))
# labels[hm_df1$colInd]

save.image("data/demo.RData")
