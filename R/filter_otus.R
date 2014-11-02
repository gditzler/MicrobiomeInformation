#' Filter OTU maktrix by their abundance
#' 
#' Many of the overall abundances for different taxa are quite low, so in this
#' function, we filter them to a smaller subset that contain only the variables
#' that are the most abundant up to some pre-determined level. 
#' 
#' @param biom_df data.frame that has be OTU data
#' @param lvl percentage of abundance to keep 
#' @return return the biom_df with a subset of OTUs 
filter_otus <- function(biom_df, lvl=0.2) {
  if (lvl > 1 || lvl < 0) {
    stop("lvl must be between zero and one.")
  }
  # determine the total abundance for each of the features 
  rsum <- rowSums(biom_df$data_dense)
  rsum <- rsum/sum(rsum)
  idx <- sort(rsum, decreasing=TRUE, index.return=TRUE)
  idx <- idx$ix
  
  # begin to search for lvl % of features that should be kept around
  cumsum <- 0
  n <- 1
  idx_keep <- NULL
  while(TRUE) {
    if (rsum[idx[n]]+cumsum <= lvl) {
      cumsum <- cumsum + rsum[idx[n]]
      n <- n+1
      idx_keep <- c(idx_keep, idx[n])
    } else {
      break
    }
  }
  
  data <- matrix(data=0, nrow=length(idx_keep), ncol=biom_df$shape[2])
  otu_names <- NULL
  otu_ids <- NULL 
  for (n in 1:length(idx_keep)) {
    data[n,] <- biom_df$data_dense[idx_keep[n],]
    otu_names <- c(otu_names, biom_df$otu_names[n])
    otu_ids <- c(otu_ids, biom_df$otu_ids[n])
  }
  biom_df$data_dense <- NULL
  biom_df$otu_names <- NULL
  biom_df$otu_ids < NULL
  biom_df$data_dense <- data
  biom_df$otu_names <- otu_names
  biom_df$otu_ids < otu_ids
  print(otu_ids)
  return(biom_df)
}
