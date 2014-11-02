#' Load a map file in tab-delimited format
#' 
#' @param map_fp string pointing to the path of the map file.
#' @example
#' \donotrun{
#' meta_map <- load_metadata("~/Git/DataCollections/Caporaso/caporaso-gut.txt")
#' }
load_metadata <- function(map_fp) {
  df <- read.delim(map_fp, sep="\t")
  return(df)
}