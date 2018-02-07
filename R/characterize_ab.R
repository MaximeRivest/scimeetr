#' Make a list of dataframe of abstract word frequency
#' 
#' This can be used to characterize the sub-communities found by scimap().
#'
#' @param scimeetr_data A scimeetr object
#' @return A list of dataframe of length equal to the number of sub-communities
#' @importFrom dplyr %>%
#' @export
characterize_ab <- function(scimeetr_data, lambda = 0.4) {
  hold <- purrr::map(scimeetr_data, function(x) {
    # Size
    community_size <- nrow(x$dfsci)
    # Table of most prolific journals
    tmp_df <- x$ab
    names(tmp_df)[1:3] <- c('abstract_word', 'frequency', 'pourcent' )
    return(tmp_df)
  })
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    purrr::compact()
  hold_relative <- purrr::map(hold[names(tmp)], function(ab_df, lambda) {
    tst <- ab_df %>%
      mutate(relevance = lambda * log(frequency/sum(frequency,na.rm = T), base = 10) + (1 - lambda) * log((frequency/sum(frequency,na.rm = T))/(Frequency/sum(Frequency,na.rm = T)), base = 10)) %>%
      select(abstract_word, Relative_frequency:relevance)
    return(tst)
  }, lambda)
  kw_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      kw_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'keyword') %>%
        arrange(desc(de_relevancy))
    } else {
      kw_df[[x]] <- hold[[x]]
    }
  }
  names(kw_df) <- names(scimeetr_data)
  return(kw_df)
}
return(hold)
}
