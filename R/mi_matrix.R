#' @param data matrix containing the OTU table
#' @param discrete discretize the data. default is TRUE
#' @param disc method of performing discretization. available: equalfreq, 
#' equalwidth, globalequalwidth. the default is equalwidth
#' @param nbins number of bins to discretize into 
#' @param method  method of performing mutual information calculation.
#' @return returns a matrix with the pairwise mutual information
#' @keywords mutual information
#' @examples
#' mi <- mi_matrix(data, labels, discrete=TRUE, disc="equalwidth", nbins=25, method="emp")
#' @author Gregory Ditzler
#' @title Compute the matrix of MI
#' @description
#' Calculate the pairwise mutual information for each of the features in 
#' data. 
mi_matrix <- function(data, discrete=TRUE, disc="equalwidth", 
                      nbins=25, method="emp") {
  nfeat <- length(data[,1])
  mi <- matrix(data=0, nrow=nfeat, ncol=nfeat)
  
  for (i in 1:nfeat) {
    for (j in i:nfeat) {
      if (discrete) {
        X <- discretize(data[i,], disc=disc, nbins=nbins)
        Z <- discretize(data[j,], disc=disc, nbins=nbins)
      } else {
        X <- data[i,]
        Z <- data[j,]
      }
      mi[i,j] <- mutinformation(X, Z, method=method)
      mi[j,i] <- mi[i,j]  # mi is symmetric
    }
  }
  return(mi)
}