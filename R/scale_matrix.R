#' Scale a communuity data matrix
#' 
#' This function normalizes the community data matrix, or sometimes referred to 
#' as the OTU table. This needs to be performed because multiple samples could 
#' have the same proportion of bacteria; however, the abundances are different. 
#' For example, a=[1,2,3] is proportionally equal to b=[3,6,9]. The matrix is 
#' scaled and written to df$data_dense.
#' 
#' @param biom_df data.frame from loading the biom file.
#' @param recscale if FALSE observations sum to one; otherwise they are rescaled
#' to produce integer values. default is TRUE
#' @return return the data.frame with the scaled matrix
scale_matrix <- function (biom_df, rescale=TRUE) {
  if (rescale) {
    biom_df$data_dense <- scale(biom_df$data_dense+1, center=FALSE, 
                                scale=colSums(biom_df$data_dense+1))
    biom_df$data_dense <- floor(biom_df$data_dense/min(biom_df$data_dense))
  } else {
    biom_df$data_dense <- scale(biom_df$data_dense, center=FALSE, 
                                scale=colSums(biom_df$data_dense))
  }
  return(biom_df)
}