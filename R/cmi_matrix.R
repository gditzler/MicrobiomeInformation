#' @param data matrix containing the OTU table
#' @param labels vector of discrete labels 
#' @param discrete discretize the data. default is TRUE
#' @param disc method of performing discretization. available: equalfreq, 
#' equalwidth, globalequalwidth. the default is equalwidth
#' @param nbins number of bins to discretize into 
#' @param method  method of performing mutual information calculation.
#' @return returns a matrix with the pairwise conditional mutual information
#' @author Gregory Ditzler
#' @title Compute the matrix of CMI
#' @description
#' Calculate the pairwise conditional mutual information for each of the 
#' features in data. 
cmi_matrix <- function(data, labels, discrete=TRUE, disc="equalwidth", 
                      nbins=25, method="emp") {
  nfeat <- length(data[,1])
  cmi <- matrix(data=0, nrow=nfeat, ncol=nfeat)
  for (i in 1:nfeat) {
    for (j in i:nfeat) {
      if (discrete) {
        X <- discretize(data[i,], disc=disc, nbins=nbins)
        Z <- discretize(data[j,], disc=disc, nbins=nbins)
      } else {
        X <- data[i,]
        Z <- data[j,]
      }
      # I(X;Z|Y)
      cmi[i,j] <- condinformation(X, Z, labels, method=method)
      cmi[j,i] <- cmi[i,j]  # mi is symmetric
    }
  }
  return(cmi)
}