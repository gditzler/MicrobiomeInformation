#' Read in a file in BIOM format. 
#'
#' This function reads in a biom file using the BIOM utilities. The function 
#' then converts the data from a sparse to dense format.
#'
#' @param biom_fp string that points to the path of the biom file
#' @return returns a data.frame that has the result from the biom & dense matrix
#' @examples
#' \donotrun{
#' df <- load_biom_matrix("~/Git/DataCollections/Caporaso/caporaso-gut.biom")
#' }
load_biom_matrix <- function(biom_fp) {
  df <- read_biom(biom_fp)  # requires the biom packges are installed  
  biom_data <- matrix(data=0, nrow=df$shape[1], ncol=df$shape[2])
  
  for (data_entry in df$data) {
    # remember that R indexed at 1
    biom_data[data_entry[1]+1,data_entry[2]+1] <- data_entry[3]
  }
  df$data_dense <- biom_data
  
  otu_names <- NULL
  otu_ids <- NULL
  for (i in 1:df$shape[1]) {
    otu_names <- c(otu_names, paste(df$rows[i][[1]]$metadata$taxonomy, collapse=";"))
    otu_ids <- c(otu_ids, df$rows[i][[1]]$id)
  }
  df$otu_names <- otu_names 
  df$otu_ids <- otu_ids
  
  sample_ids <- NULL
  for (j in 1:df$shape[2]) {
    sample_ids <- c(sample_ids, df$columns[j][[1]]$id)
  }
  df$sample_ids <- sample_ids
  return(df)
}
