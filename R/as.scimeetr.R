#' From data frame to scimeetr
#'
#' \code{as.scimeetr()} let's you make a scimeetr object from a valid data
#' frame. To be valid a data frame must contain at least the 14 following
#' columns: CR (cited references), UT (unique id), C1 (authors' affiliations),
#' DE (author keywords), ID (index keywords), AU (authors), JI (abbreviated
#' source title), SO (source title), VL (volume), PY (publication year), BP
#' (page start), TI (title), AB (abstract), TC (total citation). See
#' \code{scimeetr_list$com1$dfsci} for the appropriate format of each of these
#' columns.
#' @param dfsci A dataframe, which can be derived from a data frame contained
#'   within a scimeetr object.
#' @return An object of class scimeetr.
#' @seealso \code{\link{scimeetr}}, \code{\link{import_wos_files}} and
#'   \code{\link{import_scopus_files}}.
#' @examples
#' sub_dfsci <- subset(scimeetr_list$com1$dfsci, TC > 3)
#' my_scimeetr_obj <- as.scimeetr(sub_dfsci)
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
