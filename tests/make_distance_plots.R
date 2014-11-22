library("devtools")
library("ggplot2")
library("gplots")
library("reshape2")
library("plyr")
library("fields")
library("cluster")

lvl = 0.75                # filter level for OTUs



# set the paths of the biom & map files then load them 
biom_fps <- c(#"~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet.biom",
              #"~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Sex.biom",
              "~/Git/DataCollections/Caporaso/caporaso-gut.biom"
)
map_fps <- c(#"~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Diet-OV.txt",
             #"~/Git/DataCollections/AmericanGut/AmericanGut-Gut-Sex.txt",
             "~/Git/DataCollections/Caporaso/caporaso-gut.txt"
)
d_names <- c(#"ag-diet-ov",
             #"ag-gut-sex",
             "cap-gut-sex"
)
col_names <- c(#"DIET_TYPE",
               #"SEX",
               "SEX"
)

for (n in 1:length(biom_fps)) {
  
  # get the latest files for plotting 
  biom_fp <- biom_fps[n]
  map_fp <- map_fps[n]
  d_name <- d_names[n]
  col_name <- col_names[n]
  
  print(paste("Running", d_name, col_name))
  
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
  otus_filter <- lst$otu_names
  
  
  d <- as.matrix(daisy(t(data_filter)))
  pdf(paste("data/plots/",d_name,"-partial-distance.pdf", sep=""))
  image.plot(1:length(d[1,]), 1:length(d[1,]), d, xlab="", ylab="")
  dev.off()
  
  d <- as.matrix(daisy(t(data)))
  pdf(paste("data/plots/",d_name,"-full-distance.pdf", sep=""))
  image.plot(1:length(d[1,]), 1:length(d[1,]), d, xlab="", ylab="")
  dev.off()
  
}