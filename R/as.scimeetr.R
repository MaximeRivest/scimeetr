#' From data frame to scimeetr
#' 
#' @param dfsci a dataframe derived from scimeetr
#' @return an object of class scimeetr.
#' @export
as.scimeetr <- function(dfsci) {
  if(stringr::str_detect(dfsci$UT[1], 'WOS')){
    dfsci <- dplyr::select(dfsci, PT:RECID)
  } else {
    dfsci <- dplyr::select(dfsci, AU:CR)
  }
  lsci <- list("com1" = list("dfsci" = dfsci[!duplicated(dfsci), ]))
  class(lsci) <- c('scimeetr', class(lsci))
  lsci <- add_table_freq(lsci)
  return(lsci)
}
  