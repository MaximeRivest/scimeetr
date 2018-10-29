#' Characterize bibliometric corpus with countries
#' 
#' \code{characterize_co} calculates several country bibliometrics from a 
#' scimeetr object. The results are returned in a list of data frame. The 
#' metrics in the table are: frequency, relative frequency and relevance.
#' 
#' @seealso \code{\link{characterize_kw}} for keyword characterization, 
#'   \code{\link{characterize_ti}} for title-word characterization, 
#'   \code{\link{characterize_ab}} for abstract-word characterization, 
#'   \code{\link{characterize_au}} for author characterization, 
#'   \code{\link{characterize_un}} for university characterization, 
#'   \code{\link{characterize_jo}} for journal characterization
#' @param scimeetr_data An object of class scimeetr.
#' @param lambda A number from 0 to 1. If 0 the relevance score would be equal 
#'   to the relative frequency. If 1 for the relevance score would be equal to
#'   the frequency.
#' @examples 
#' # Example with an object of class scimeetr (see import_wos_files() or 
#' # import_scopus_files()) already in the workspace
#' countries <- characterize_co(scimeetr_list)
#' # Since this example shows how to load WOS from your system we need to run 
#' # the following line to find the path to the exemple file
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' # Then we can run the actual example
#' example_scimeetr_object <- import_wos_files(files_directory = fpath)
#' characterize_co(example_scimeetr_object)
#' 
#' @return A list of dataframe. The list length matchs the number of communities
#'   that the scimeetr object contains.
#' @import dplyr
#' @export
characterize_co <- function(scimeetr_data, lambda = 0.5) {
  hold <- purrr::map(scimeetr_data, function(x, splitted_cr) {
    C1vec <- x$dfsci$C1
    country_final_vec <- rep(NA, nrow(x$dfsci))
    for (i in 1:nrow(x$dfsci)) {
      if(length(stringr::str_extract_all(C1vec[i], pattern = "[:alnum:]{1,};\\s\\[")[[1]]) != 0){
        univ1 <- stringr::str_locate_all(C1vec[i], pattern = "[:alnum:]{1,};\\s\\[")
        univ1[[1]][,2] <- univ1[[1]][,2] - 3
        univ2 <- stringr::str_extract(C1vec[i], pattern = "\\w*$")
        univec <- sapply(row(univ1[[1]])[,1], function(x){substr(C1vec[i], univ1[[1]][x,1], univ1[[1]][x,2])})
        univec <- unique(c(univec, univ2))
        if(!all(is.na(stringr::str_extract(univec, pattern = "[:digit:]")))) {
          country_final_vec[i] <- "USA"
        } else {
          country_final_vec[i] <- paste(univec, collapse = ";")
        }
      } else if (length(stringr::str_extract_all(C1vec[i], pattern = "\\w*$")[[1]]) != 0) {
        univ1 <- stringr::str_extract(C1vec[i], pattern = "\\w*$")
        univ2 <- stringr::str_locate_all(C1vec[i], pattern = "[:alpha:]{1,};")
        if(nrow(univ2[[1]]) != 0 & length(stringr::str_extract(C1vec[i], pattern = "\\]")) == 0 ) { 
          univ2[[1]][,2] <- univ2[[1]][,2] - 1
          univ2 <- sapply(row(univ2[[1]])[,1], function(x){substr(C1vec[i], univ2[[1]][x,1], univ2[[1]][x,2])})
        } else { univ2 <- NULL}
        univec<- unique(c(univ1, univ2))
        if(!any(is.na(stringr::str_extract(univec, pattern = "[:digit:]")))) {
          country_final_vec[i] <- "USA"
        } else {
          country_final_vec[i] <- paste(univec, collapse = ";")
        }
      }
    }
    co <- unlist(stringr::str_split(country_final_vec, ';'))
    df <- as.data.frame(table(tolower(co)), stringsAsFactors = F)
    co <- arrange(df, desc(Freq))
    return(co)
  })
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    compact()
  hold_relative <- purrr::map2(hold[names(tmp)], hold[as.character(tmp)], function(child, parent, lambda) {
    tst <- left_join(child, parent, by = "Var1") %>%
      mutate(Freq_rel = (Freq.x / Freq.y) / (sum(Freq.x,na.rm = T)/sum(Freq.y, na.rm = T)),
             relevance = lambda * log(Freq.x/sum(Freq.x,na.rm = T), base = 10) + (1 - lambda) * log((Freq.x/sum(Freq.x,na.rm = T))/(Freq.y/sum(Freq.y,na.rm = T)), base = 10)) %>%
      select(Var1,Freq_rel:relevance)
  }, lambda)
  co_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      co_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'Var1') %>%
        arrange(desc(relevance))
      names(co_df[[x]]) <- c('country',
                             'frequency',
                             'relative_frequency',
                             'relevance')
    } else {
      co_df[[x]] <- hold[[x]]
      names(co_df[[x]]) <- c('country',
                             'frequency')
    }
  }
  names(co_df) <- names(scimeetr_data)
  return(co_df)
}
