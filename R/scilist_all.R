#' Make all reading lists
#' 
#' @param scimeetr_data A scimeetr object. This is created from importing files 
#'   with import_wos_file or scimap().
#' @param length_list An integer that will multiply the standard list length.
#' @return a list of reading list of the same length as the number of
#'   communities found in the input scimeetr object
#' @export
#' @import dplyr purrr
scilist_all <- function(scimeetr_data, length_list = 1) {
  c1 <- scilist(scimeetr_data, k = ceiling(10 * length_list))
  c2 <- scilist(scimeetr_data, reading_list = 'core_yr', k = ceiling(1*length_list))
  c3 <- scilist(scimeetr_data, reading_list = 'core_residual', k = ceiling(4*length_list))
  bex <- scilist(scimeetr_data, reading_list = 'by_expert_LC', k = ceiling(5*length_list), m = 2)
  gex <- scilist(scimeetr_data, reading_list = 'group_of_experts_LC', k = ceiling(4*length_list))
  co1 <- scilist(scimeetr_data, reading_list = 'cite_most_others', k = ceiling(6*length_list))
  co2 <- scilist(scimeetr_data, reading_list = 'betweeness', k = ceiling(4*length_list))
  co3 <- scilist(scimeetr_data, reading_list = 'connectness', k = ceiling(4*length_list))
  co4 <- scilist(scimeetr_data, reading_list = 'closeness', k = ceiling(4*length_list))
  co5 <- scilist(scimeetr_data, reading_list = 'page_rank', k = ceiling(4*length_list))
  co6 <- scilist(scimeetr_data, reading_list = 'direct_cite_eigen', k = ceiling(8*length_list))
  co7 <- scilist(scimeetr_data, reading_list = 'link_strength', k = ceiling(4*length_list))
  rl <- list(c1,c2,c3,bex,gex,co1,co2,co3,co4,co5,co6,co7)
  rl <- purrr::transpose(rl)
  rl <- purrr::map(rl, function(x) {
    purrr::map(x, function(y){
      y$metric <- as.character(y$metric)
      y$publication <- as.character(y$publication)
      y$list_type <- as.character(y$list_type)
      return(y)})})
  rl <- purrr::map(rl, dplyr::bind_rows)
  return(rl)  
}