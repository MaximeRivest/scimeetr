#' Finds research communities
#' 
#' @param scimeetr_list A list of class scimeetr. Objects of class scimeetr are return by 
#' with import_bib_file or a list of class scimeetr created from the 
#' function scimap.
#' @param coupling A vector of length one. Equal to either: 
#' 'bc'/'bibliographic coupling', 'kc'/'keyword coupling', 'tc'/'title coupling', 
#' 'ac'/'abstract coupling', 'jc'/'journal coupling', 'ac'/'author coupling'.
#' @param community_algorithm A vector of length one. 'louvain', 'fast greedy'...
#' @param inclusive_com TRUE or FALSE. Tells wheter to include within the subcommunities
#' all the frontier nodes that were not originally included in the subcommunities.
#' @return biblio_list. A list of the communities, including the global community,
#'  with all their associated graph object, community object and biblio_df object.
#' @export
#' @import dplyr purrr

scimap <- function(scimeetr_list = scimeetr_list, 
                   coupling_by = 'bic',
                   community_algorithm = 'louvain',
                   min_com_size = 30) {
  names_lsci <- names(scimeetr_list)
  lsci_temp <- list()
  for(i in 1:length(scimeetr_list)){
    i_lsci <- scimeetr_list[[i]]
    if(sum(stringr::str_detect(names(i_lsci), 'graph')) >= 1){
      i_lsci <- list(i_lsci)
      names(i_lsci) <- names_lsci[i]
      lsci_temp <- c(lsci_temp, i_lsci)
    } else {
      graph_object = coupling(i_lsci$dfsci, coupling_by)
      igraph::E(graph_object)$weight <- log(igraph::E(graph_object)$weight + 1, 100)
      coms = clusterize(graph_object, community_algorithm)
      utcom <- data.frame(UT = names(igraph::membership(coms)),
                          MB = as.vector(igraph::membership(coms)),
                          stringsAsFactors = F)
      i_lsci$dfsci <- i_lsci$dfsci %>%
        left_join(utcom, by = c('UT'))
      i_lsci <- list(c(i_lsci,
                        list("graph" = graph_object,
                             "coms" = coms)))
      names(i_lsci) <- names_lsci[i]
      lsci_temp <- c(lsci_temp, i_lsci)
      umem <- unique(i_lsci[[1]]$dfsci$MB)
      for(com_n in umem){
        temp_dfsci <- filter(i_lsci[[1]]$dfsci, MB == com_n)
        if(nrow(temp_dfsci) >= min_com_size) {
          temp_dfsci <- select(temp_dfsci, -MB)
          temp_dfsci <- list(list("dfsci" = temp_dfsci,
                                  "parent_com" = names_lsci[i]))
          names(temp_dfsci) <-  paste0(names_lsci[i], "_",com_n)
          lsci_temp <- c(lsci_temp, temp_dfsci)
        }
      }
    }
  }
  class(lsci_temp) <- c('scimeetr', class(lsci_temp))
  lsci_temp <- add_table_freq(lsci_temp)
  return(lsci_temp)
}

