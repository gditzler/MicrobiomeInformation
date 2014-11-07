#' @param map_df map data.frame from load_metadata
#' @param sample_ids list of sample ids containing the order of the samples 
#' in the data matrix
#' @param col_name column name in the map file to extract the labels
#' @author Gregory Ditzler
#' @title Extract labels from a map data.frame
retrieve_labels <- function(map_df, sample_ids, col_name) {
  map_df$retrieve_labels <- map_df[,col_name]
  labels <- NULL
  for (j in 1:length(sample_ids)) {
    value <- map_df$retrieve_labels[map_df$X.SampleID == sample_ids[j]]
    labels <- c(labels, value)
  }
  return(labels)
}