#' Make a list of dataframe of keyword frequency
#' 
#' This can be used to characterize the sub-communities found by scimap().
#'
#' @param scimeetr_data A scimeetr object
#' @return A plot, generated from the sankeyNetwork function in the package networkD3
#' @importFrom dplyr %>%
#' @export
characterize_kw <- function(scimeetr_data, n = 10, nb_of_underscore = 1) {
  lsci21 <- keep(scimeetr_data, stringr::str_count(names(scimeetr_data), "_") == nb_of_underscore)
  hold <- map(lsci21,
              function(x) {
                data.frame(community_size = x$de$comsize[1],
                           most_frequent_keywords = x$de[1:n,]$ID,
                           disproportionately_more_frequent = arrange(x$de[1:round(x$de$comsize[1] * 0.5),], desc(Relative_frequency * log(Frequency.x, 5)))$ID[1:n],
                           relative_frequency_1 = arrange(x$de[1:round(x$de$comsize[1] * 0.5),], desc(Relative_frequency * log(Frequency.x, 5)))$Relative_frequency[1:n],
                           disproportionately_less_frequent = arrange(x$de[1:round(x$de$comsize[1] * 0.5),], Relative_frequency *log(Frequency.x, 5))$ID[1:n],
                           relative_frequency_2 = arrange(x$de[1:round(x$de$comsize[1] * 0.5),], Relative_frequency *log(Frequency.x, 5))$Relative_frequency[1:n],
                           most_frequent_publishers = names(sort(table(x$dfsci$SO), decreasing = T)[1:n]),
                           frequency = as.vector(sort(table(x$dfsci$SO), decreasing = T))[1:n],
                           stringsAsFactors = F)
              }
  )
  return(hold)
}
