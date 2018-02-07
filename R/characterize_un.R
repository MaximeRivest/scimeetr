#' Make a list of dataframe of univeristy frequency
#' 
#' This can be used to characterize the communities of research.
#'
#' @param scimeetr_data A scimeetr object
#' @return A list of dataframe containing university related metrics
#' @import dplyr
#' @export
characterize_un <- function(scimeetr_data) {
  hold <- purrr::map(scimeetr_data, function(x, splitted_cr) {
    C1vec <- x$dfsci$C1
    univ_final_vec <- rep(NA, nrow(x$dfsci))
    for (i in 1:nrow(x$dfsci)) {
      if(length(stringr::str_extract_all(C1vec[i], pattern = "\\]\\s[[:alnum:]\\s]{1,}")[[1]]) != 0){
        univ1 <- stringr::str_locate_all(C1vec[i], pattern = "\\]\\s[[:alnum:]\\s]{1,}")
        univ1[[1]][,1] <- univ1[[1]][,1] + 2
        univec <- sapply(row(univ1[[1]])[,1], function(x){substr(C1vec[i], univ1[[1]][x,1], univ1[[1]][x,2])})
        univec <- unique(univec)
        univ_final_vec[i] <- paste(univec, collapse = ";")
      } else if (length(stringr::str_extract_all(C1vec[i], pattern = "^[[:alnum:]\\s]{1,}")[[1]]) != 0) {
        univ1 <- stringr::str_extract_all(C1vec[i], pattern = "^[[:alnum:]\\s]{1,}")
        univ2 <- stringr::str_locate_all(C1vec[i], pattern = ";\\s[[:alnum:]\\s]{1,}")
        if(nrow(univ2[[1]]) != 0) { 
          univ2[[1]][,1] <- univ2[[1]][,1] + 2
          univ2 <- sapply(row(univ2[[1]])[,1], function(x){substr(C1vec[i], univ2[[1]][x,1], univ2[[1]][x,2])})
        } else { univ2 <- NULL}
        univec<- unique(c(univ1, univ2))
        univ_final_vec[i] <- paste(univec, collapse = ";")
      }
    }
    co <- unlist(stringr::str_split(univ_final_vec, ';'))
    return(arrange(as.data.frame(table(tolower(co)),stringsAsFactors = F), desc(Freq)))
  })
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    compact()
  hold_relative <- purrr::map2(hold[names(tmp)], hold[as.character(tmp)], function(child, parent) {
    tst <- left_join(child, parent, by = "Var1") %>%
      mutate(Freq_rel = (Freq.x / Freq.y) / (sum(Freq.x,na.rm = T)/sum(Freq.y, na.rm = T))) %>%
      select(Var1,Freq_rel)
  })
  co_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      co_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'Var1')
    } else {
      co_df[[x]] <- hold[[x]]
    }
  }
  names(co_df) <- names(scimeetr_data)
  return(co_df)
}
