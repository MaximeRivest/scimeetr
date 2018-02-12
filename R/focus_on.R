#' Focus on only one community
#' 
#' @param scimeetr_list A \link{scimeetr} object.
#' @param grab A character string matching the name of the community that you
#' which to focus on.
#' @return A scimeetr object.
#' @export

focus_on <- function(scimeetr_list, grab) {
  scimeetr_list <- scimeetr_list[stringr::str_detect(names(scimeetr_list), grab)]
  class(scimeetr_list) <- c('scimeetr', 'list')
  return(scimeetr_list)
}