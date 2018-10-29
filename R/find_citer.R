#' Find publication citer
#'
#' @param lsci a scimeetr object
#' @param publication a vector of length one matchin the name of a publication from scilist output
#' @return A data.frame
#' @export
#' @import dplyr purrr
find_citer <- function(lsci, publication) {
  cr_vec <- toupper(lsci[[1]][[1]]$CR)
  publication <- paste(stringr::str_split(publication, ', ')[[1]][1:3],collapse = ', ')
  dfsci <- lsci[[1]][[1]][stringr::str_detect(cr_vec, publication),]
  return(dfsci)
}
