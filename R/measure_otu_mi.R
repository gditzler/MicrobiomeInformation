#' Measure the mutual information in the OTU table
#' 
#' This function measures the mutual information shared between the OTU 
#' variables and the class labels that have been extracted from the map file
#' describing the metadata.
#' 
#' @param biom_df data.frame containing the OTU table
#' @param labels class labels for each of the samples in the OTU table
#' @param discrete discretize the data. default is FALSE
#' @param disc method of performing discretization. available: equalfreq, 
#' equalwidth, globalequalwidth. the default is equalfreq
#' @param nbins number of bins to discretize into 
#' @param method  method of performing mutual information calculation.
#' @return returns a vector with the mutual information
measure_otu_mi <- function(biom_df, labels, discrete=FALSE, disc="equalfreq", 
                           nbins=25, method="emp") {
  mi <- NULL
  for (i in 1:length(biom_df$data_dense[,1])) {
    if (discrete) {
      X <- discretize(biom_df$data_dense[i,], disc=disc, nbins=nbins)
    } else {
      X <- biom_df$data_dense[i,]
    }
    mi <- c(mi, mutinformation(X, labels, method=method))
  }
  return(mi)
}