#' Dive to a sub-community and keep its sub-communities
#' 
#' @param scimeetr_list A \link{scimeetr} object
#' @param aim_at A character string matching the name of the community that you
#' which to get to
#' @return A scimeetr object.
#' @export

dive_to <- function(scimeetr_list, aim_at) {
  scimeetr_list <- scimeetr_list[stringr::str_detect(names(scimeetr_list), paste0(aim_at, ".*"))]
  class(scimeetr_list) <- c('scimeetr', 'list')
  return(scimeetr_list)
}