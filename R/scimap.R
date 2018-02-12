#' Finds research communities
#' 
#' \code{scimap} is used to find sub-communities of a \link{scimeetr} object.
#' 
#' @param scimeetr_data A list of class scimeetr. Objects of class scimeetr are 
#'   returned by import_wos_files or by scimap itself.
#' @param coupling_by A character vector of length one. Equal to either: 'bic' 
#'   for bibliographic coupling, 'kec' for keyword coupling, 'tic' for title 
#'   coupling, 'abc' for abstract coupling, 'joc' for journal coupling, 'auc' 
#'   for author coupling', 'woc' for word coupling, 'bickec' for a combination
#'   of bibliographic coupling and keyword coupling, and 'bickecticjoc' for a
#'   combination of bic, kec, tic, joc (this might be the better way for most
#'   use but it is a bit slow, this is why it is not the default choice).
#' @param community_algorithm A character vector of length one. 'louvain', 'fast
#'   greedy'...
#' @return scimeetr_data. A \link{scimeetr} object.
#' @export
#' @import dplyr purrr
#' @examples 
#' # Here we see that scimeetr_list contains no sub-communities
#' summary(scimeetr_list)
#' sci <- scimap(scimeetr_list, coupling_by = 'bickec')
#' # After applying scimap() to scimeetr_list. The resulting variable (sci)
#' # contains sub-communities
#' summary(sci)

scimap <- function(scimeetr_data, coupling_by = 'bic',
                   community_algorithm = 'louvain', min_com_size = 30, kw = 1,
                   ti = 1, ab = 1) {
  names_lsci <- names(scimeetr_data)
  lsci_temp <- list()
  if(coupling_by == 'joc' | coupling_by == 'bickecticjoc') {
    splitted_cr <- split_cr(scimeetr_data, min_cr_freq = 0)
  } else {
    splitted_cr <- NULL
  }
  for(i in 1:length(scimeetr_data)){
    i_lsci <- scimeetr_data[[i]]
    if(sum(stringr::str_detect(names(i_lsci), 'graph')) >= 1){
      i_lsci <- list(i_lsci)
      names(i_lsci) <- names_lsci[i]
      lsci_temp <- c(lsci_temp, i_lsci)
    } else {
      graph_object = coupling(i_lsci$dfsci, coupling_by, kw, ti, ab, splitted_cr)
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

