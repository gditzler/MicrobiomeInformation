#' @param map_fp string pointing to the path of the map file.
#' @return data.frame containing the metadata
#' @author Gregory Ditzler
#' @title Load a map file in tab-delimited format
load_metadata <- function(map_fp) {
  df <- read.delim(map_fp, sep="\t")
  return(df)
}