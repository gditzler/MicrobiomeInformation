# build the documentation and load the package into the environment
library("devtools")
library("ggplot2")
library("gplots")
library("reshape2")
library("plyr")
library("fields")

document()
load("data/demo.RData") # optional 

# set up program constants
col_name = "DIET_TYPE"   # column in the map files for the labels
lvl = 0.75                # filter level for OTUs
nbins = 50               # number of bins for estimating the pdfs

# set the paths of the biom & map files then load them 
biom_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet.biom"
map_fp <- "~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet-OV.txt"
d_name <- "ag-diet-ov"


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
data_filter <- lst$data

# compute mi & cmi matrices
mi <- mi_matrix(data_filter, discrete = TRUE, disc = "equalwidth", nbins = nbins, method = "emp")
cmi <- cmi_matrix(data_filter, labels, discrete = TRUE, disc = "equalwidth", nbins = nbins, method = "emp")

# measure the OTUs MI and CMI
mi_vec <- measure_otu_mi(data, labels, discrete=TRUE, disc="equalwidth", nbins=nbins, method="emp")
cmi_vec <- measure_otu_cmi(data, labels, discrete=TRUE, disc="equalwidth", nbins=nbins, method="emp")
mi_vec_f <- measure_otu_mi(data_filter, labels, discrete=TRUE, disc="equalwidth", nbins=nbins, method="emp")
cmi_vec_f <- measure_otu_cmi(data_filter, labels, discrete=TRUE, disc="equalwidth", nbins=nbins, method="emp")


hist(mi_vec, xlab="mutual information", main="Histogram of Mutual Information")
hist(cmi_vec, xlab="conditional mutual information", main="Histogram of Conditional Mutual Information")
hist(mi_vec_f, xlab="mutual information", main="Histogram of Mutual Information")
hist(cmi_vec_f, xlab="conditional mutual information", main="Histogram of Conditional Mutual Information", prob=T)


bin_w = 0.0007
ggplot(data.frame(x=1:length(mi_vec), mi=mi_vec), aes(x=mi))+
  geom_histogram(aes(y=..density.., fill=..count..), colour='black', binwidth=bin_w)#+
  #geom_line(stat="density", colour='blue',size=2)
ggsave(file=paste("data/plots/",d_name,"-density-mi-full.pdf",sep=""))

ggplot(data.frame(x=1:length(cmi_vec), cmi=cmi_vec), aes(x=cmi))+
  geom_histogram(aes(y=..density.., fill=..count..), colour='black', binwidth=bin_w)#+
  #geom_line(stat="density", colour='blue',size=2)
ggsave(file=paste("data/plots/",d_name,"-density-cmi-full.pdf",sep=""))


ggplot(data.frame(x=1:length(mi_vec_f), mi=mi_vec_f), aes(x=mi))+
  geom_histogram(aes(y=..density.., fill=..count..), colour='black', binwidth=bin_w)#+
  #geom_line(stat="density", colour='blue',size=2)
ggsave(file=paste("data/plots/",d_name,"-density-mi-partial.pdf",sep=""))


ggplot(data.frame(x=1:length(cmi_vec_f), cmi=cmi_vec_f), aes(x=cmi))+
  geom_histogram(aes(y=..density.., fill=..count..), colour='black', binwidth=bin_w)#+
  #geom_line(stat="density", colour='blue',size=2)
ggsave(file=paste("data/plots/",d_name,"-density-cmi-partial.pdf",sep=""))


# what to plot...
image.plot(1:length(mi[1,]), 1:length(mi[,1]), log(mi), xlab="", ylab="")
image.plot(1:length(mi[1,]), 1:length(cmi[,1]), log(cmi), xlab="", ylab="")

data2 <- data_filter
data2 <- data2 / t(replicate(nrow(data2), colSums(data2)))

hm_df1 <- heatmap(data_filter*10000+1, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'canberra'))
hm_df2 <- heatmap(data_filter, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'binary'))
hm_df3 <- heatmap(data_filter, col=cm.colors(256), Rowv = NULL, Colv = NULL, distfun=function(x) dist(x,method = 'euclidean'))
# labels[hm_df1$colInd]

save.image("data/demo.RData")

