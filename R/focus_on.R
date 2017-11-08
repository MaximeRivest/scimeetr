#' Focus on only one community
#' 
#' @param scimeetr_list a scimeetr object
#' @param grab a character string matching the name of the community that you
#' which to focus on
#' @return a scimeetr object
#' @export

focus_on <- function(scimeetr_list, grab) {
  scimeetr_list <- scimeetr_list[stringr::str_detect(names(scimeetr_list), grab)]
  class(scimeetr_list) <- c('scimeetr', 'list')
  return(scimeetr_list)
}