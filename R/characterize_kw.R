#' Make a list of dataframe of keyword frequency
#' 
#' This can be used to characterize the sub-communities found by scimap().
#'
#' @param scimeetr_data A scimeetr object
#' @return A plot, generated from the sankeyNetwork function in the package networkD3
#' @importFrom dplyr %>%
#' @export
characterize_kw <- function(scimeetr_data) {
  lsci21 <- keep(scimeetr_data, stringr::str_count(names(scimeetr_data), "_") == 1)
  hold <- map(lsci21,
              function(x) {
                data.frame(community_size = x$de$comsize[1],
                           most_frequent_keywords = x$de[1:10,]$ID,
                           disproportionately_more_frequent = arrange(x$kw[1:round(x$de$comsize[1] * 0.1),], desc(Relative_frequency * log(Frequency.x, 5)))$ID[1:10],
                           disproportionately_less_frequent = arrange(x$kw[1:round(x$de$comsize[1] * 0.1),], Relative_frequency *log(Frequency.x, 5))$ID[1:10],
                           most_frequent_publishers = names(sort(table(x$dfsci$SO), decreasing = T)[1:10]),
                           stringsAsFactors = F)
              }
  )
  return(hold)
}
