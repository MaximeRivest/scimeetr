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
merge_scimaps <- function(sl, minimum_com_overlap, min_com_size = 15) {
  idmaker <- function(x){
    max.val = x*10000
    count <- nchar(as.character(max.val))
    size <- paste("%0",count,"d",sep="")
    lets <- toupper(sample(letters,x, replace=T))
    nums <- sprintf(size,sample(1:max.val)[1])
    ids <- paste(lets,nums,sep="")
    return(ids)
  }
  #Make a data frame with one column containing WOS id and one containing 
  #community membership. Make sure MB number from one scimap is different for
  #those of other scimap.
  wos2mb <- purrr::map(sl, function(sci_i){
    idmaker(1)
    tmp <- dplyr::select(sci_i[[1]]$dfsci, UT, MB)
    tmp$MB <- paste0(tmp$MB, idmaker(1), sep='_')
    return(tmp)
  })
  mbutdf <- dplyr::bind_rows(wos2mb)
  
  #Couple and make a graph
  couple_df <- inner_join(mbutdf, mbutdf, by = 'MB') %>%
    filter(UT.x > UT.y) %>%
    group_by(UT.x, UT.y) %>%
    summarise(count = n()) %>%
    filter(count >= minimum_com_overlap) %>%
    select(rec1 = UT.x,rec2 = UT.y, weight = count)
  rm(mbutdf)
  couple_df <- ungroup(couple_df)
  tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
  if(length(tmp) >= 1){
    missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                             'rec2' = dfsci$UT[1],
                             'weight' = 0,
                             stringsAsFactors = F)
    couple_df <- rbind(couple_df, missing_df)
  }
  graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
  
  #Make the community object
  coms = igraph::cluster_louvain(graph)
  utcom <- data.frame(UT = names(igraph::membership(coms)),
                      MB = as.vector(igraph::membership(coms)),
                      stringsAsFactors = F)
  #Put back together in a list of class scimeetr.
  dfsci <- select(sl[[1]]$com1$dfsci, -MB)
  lsci <- as.scimeetr(dfsci)
  lsci$com1$dfsci <- lsci$com1$dfsci %>%
    left_join(utcom, by = c('UT'))
  lsci <- list(c(lsci[[1]],
                 list("graph" = graph,
                    "coms" = coms)))
  names(lsci) <- c('com1')
  lsci_temp <- list()
  lsci_temp <- c(lsci_temp, lsci)
  class(lsci_temp) <- c('scimeetr', 'list')
  umem <- unique(lsci[[1]]$dfsci$MB)
  for(com_n in umem){
    temp_dfsci <- filter(lsci[[1]]$dfsci, MB == com_n)
    if(nrow(temp_dfsci) >= min_com_size) {
      temp_dfsci <- select(temp_dfsci, -MB)
      temp_dfsci <- list(list("dfsci" = temp_dfsci,
                              "parent_com" = 'com1'))
      names(temp_dfsci) <-  paste0('com1', "_",com_n)
      lsci_temp <- c(lsci_temp, temp_dfsci)
    }
  }
  class(lsci_temp) <- c('scimeetr', 'list')
  lsci_temp <- scimeetr:::add_table_freq(lsci_temp)
  return(lsci_temp)
}
