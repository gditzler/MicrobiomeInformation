#' @param biom_df data.frame containing the OTU table
#' @param labels class labels for each of the samples in the OTU table
#' @param n_select number of OTUs to select 
#' @param discrete discretize the data. default is FALSE
#' @param disc method of performing discretization. available: equalfreq, 
#' equalwidth, globalequalwidth. the default is equalfreq
#' @param nbins number of bins to discretize into 
#' @param method  method of performing mutual information calculation.
#' @return returns a data.frame containing the highest ranking indices 
#' according to mutual information 
#' @author Gregory Ditzler
#' @title Mutual Information Maximization
mi_selection <- function (biom_df, labels, n_select, discrete=FALSE, 
                          disc="equalfreq", nbins=25, method="emp") {

  mis <- NULL
  if (discrete) {
    for (i in 1:length(data[,1])) {
      X <- discretize(data[i,], disc=disc, nbins=nbins)
      mis <- c(mis, mutinformation(X, labels, method=method))
    }
  } else {
    for (i in 1:length(data[,1])) {
      mis <- c(mis, mutinformation(data[i,], labels, method=method))
    }
  }
  
  
  idx <- sort(mis, decreasing=TRUE, index.return=TRUE)
  idx_important <- idx[1:n_select]
  otu_df <- data.frame(otuID=biom_df$otu_ids[idx_important], 
                       otuNames=biom_df$otu_names[idx_important],
                       MI=mis[idx_important],
                       row.names=idx_important)
  return (otu_df)
}