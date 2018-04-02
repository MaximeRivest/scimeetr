#' Merge scimaps
#' 
#' With \code{merge_scimaps} you can re-assign communities to papers based on a 
#' conscensus from different coupling options.
#' 
#' @param sl A list of \link{scimeetr} object
#' @param minimum_com_overlap An integer. Minimum weight of the link between 
#'   paper to keep in the graph. If it is equal to length(sl) that means that 
#'   papers that were not classifies in the same communities for every scimap 
#'   would not be linked in a network.
#' @param min_com_size An integer.
#' @return A list a class scimeetr.
#' @import dplyr
#' @export
#' 
merge_scopus_and_wos <- function(scopus, wos){
}