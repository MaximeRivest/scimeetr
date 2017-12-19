#' Make a list of dataframe of abstract word frequency
#' 
#' This can be used to characterize the sub-communities found by scimap().
#'
#' @param scimeetr_data A scimeetr object
#' @return A list of dataframe of length equal to the number of sub-communities
#' @importFrom dplyr %>%
#' @export
characterize_ab <- function(scimeetr_data, n = 10) {
  lsci21 <- keep(scimeetr_data, stringr::str_count(names(scimeetr_data), "_") == 1)
  hold <- map(lsci21,
              function(x) {
                data.frame(community_size = x$ab$comsize[1],
                           most_frequent_keywords = x$ab[1:n,]$ID,
                           disproportionately_more_frequent = arrange(x$ab[1:round(x$ab$comsize[1] * 0.5),], desc(Relative_frequency * log(Frequency.x, 5)))$ID[1:n],
                           relative_frequency_1 = arrange(x$ab[1:round(x$ab$comsize[1] * 0.5),], desc(Relative_frequency * log(Frequency.x, 5)))$Relative_frequency[1:n],
                           disproportionately_less_frequent = arrange(x$ab[1:round(x$ab$comsize[1] * 0.5),], Relative_frequency *log(Frequency.x, 5))$ID[1:n],
                           relative_frequency_2 = arrange(x$ab[1:round(x$ab$comsize[1] * 0.5),], Relative_frequency *log(Frequency.x, 5))$Relative_frequency[1:n],
                           stringsAsFactors = F)
              }
  )
  return(hold)
}
