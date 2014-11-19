#' @param data matrix with the OTU table
#' @param labels class labels for each of the samples in the OTU table
#' @param discrete discretize the data. default is FALSE
#' @param disc method of performing discretization. available: equalfreq, 
#' equalwidth, globalequalwidth. the default is equalwidth
#' @param nbins number of bins to discretize into 
#' @param method  method of performing mutual information calculation.
#' @return returns a vector with the mutual information
#' @author Gregory Ditzler
#' @title Measure the conditional mutual information in the OTU table
#' @description
#' This function measures the conditional mutual information shared 
#' between the OTU variables and the class labels that have been 
#' extracted from the map file describing the metadata.
measure_otu_cmi <- function(data, labels, discrete=FALSE, disc="equalwidth", 
                           nbins=25, method="emp") {
  cmi <- NULL
  for (i in 1:length(data[,1])) {
    if (discrete) {
      X <- discretize(data[i,], disc=disc, nbins=nbins)
    } else {
      X <- data[i,]
    }
    cmi <- c(cmi, condinformation(X, labels, method=method))
  }
  return(cmi)
}