#' Find papers of one community that cite most the papers in an other community
#'
#' With \code{merge_scopus_and_wos} you can merge bibliometric data from Scopus
#' and the Web of Science. Be carefull if you are using this prior to
#' bibliographic coupling. Scopus and WOS format the cited reference field
#' differently.
#'
#' @param scimeetr_data A scimeetr object.
#' @param com_citing A character string matching the name of the citing
#'   community.
#' @param com_cited A character string matching the name of the cited community.
#' @return A dataframe.
#' @details I thank Dr. Sofia van Moorsel for stimulating the creation of this
#'   function.
#' @import dplyr
#' @export
#' 
find_citers_of_other_com <- function(scimeetr_data, com_citing, com_cited){
  lev_com <- stringr::str_count(names(scimeetr_data), "_")
  splt_cr <- split_cr(scimeetr_data)
  dfsci <- scimeetr_data[[com_citing]]$dfsci
  cr_list <- strsplit(dfsci$CR, split = "; ")
  cr_df <- data.frame(
    RECID = rep(dfsci$RECID, sapply(cr_list, length)),
    DOI = rep(dfsci$DI, sapply(cr_list,length)), 
    CR = toupper(unlist(cr_list)), stringsAsFactors = F
  )
  rl <- inner_join(cr_df, splt_cr, by = c(CR = "record")) %>% 
    inner_join(scimeetr_data[[com_cited]]$dfsci, by = c(RECID.y = "RECID")) %>% 
    group_by(RECID.x) %>%
    mutate(Nb_of_ref_within_com = n()) %>% 
    arrange(desc(Nb_of_ref_within_com))
  #rl <- rl[1:k,]
  #names(rl) <- c("publication", "metric")
  # rl$list_type <- "cite_most_others"
  return(rl)
}