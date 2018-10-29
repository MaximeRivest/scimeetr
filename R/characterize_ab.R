#' Characterize words found within papers' abstract
#' 
#' \code{characterize_ab} calculates several abstract-word metrics from a
#' scimeetr object. The results are returned in a list of data frame. The
#' metrics in the table are: abstract-words frequency, abstract-words relative
#' frequency, abstract-words relevance.
#' 
#' @seealso \code{\link{characterize_jo}} for journal characterization, 
#'   \code{\link{characterize_ti}} for title-word characterization, 
#'   \code{\link{characterize_kw}} for keyword characterization, 
#'   \code{\link{characterize_au}} for author characterization, 
#'   \code{\link{characterize_un}} for university characterization, 
#'   \code{\link{characterize_co}} for country characterization
#' @param scimeetr_data An object of class scimeetr.
#' @param lambda A number from 0 to 1. 0 for relative frequency 1 for total 
#'   occurence only
#' @examples 
#' # Example with an object of class scimeetr (see import_wos_files() or 
#' # import_scopus_files()) already in the workspace
#' abstractword_list <- characterize_ab(scimeetr_list)
#' # Since this example shows how to load WOS from your system we need to run 
#' # the following line to find the path to the exemple file
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' # Then we can run the actual example
#' example_scimeetr_object <- import_wos_files(files_directory = fpath)
#' characterize_ab(example_scimeetr_object)
#' 
#' @return A list of dataframe. The list length matchs the number of communities
#'   that the scimeetr object contains.
#' @import dplyr
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
      mutate(relevance = lambda * log(frequency/sum(frequency,na.rm = T), base = 10) + (1 - lambda) * log((frequency/sum(frequency,na.rm = T))/(Frequency/sum(Frequency,na.rm = T)), base = 10))
    return(tst)
  }, lambda)
  ab_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      ab_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'abstract_word') %>%
        arrange(desc(relevance)) %>%
        select(abstract_word, frequency.x, Relative_frequency.y:relevance)
    } else {
      ab_df[[x]] <- hold[[x]]
    }
  }
  names(ab_df) <- names(scimeetr_data)
  return(ab_df)
}
