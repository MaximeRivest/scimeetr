#' Find list content in dfsci database
#'
#' @param lsci a scimeetr object
#' @param rl a reading list from scilist
#' @return A data.frame
rl2dfsci <- function(lsci, rl){
  dfsci <- dplyr::left_join(rl[[1]],lsci[[1]][[1]], by = c("publication" = "RECID"))
  return(dfsci)
}
