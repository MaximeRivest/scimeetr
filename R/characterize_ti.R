#' Characterize bibliometric corpus with titles.
#' 
#' \code{characterize_ti} calculates several title-word metrics from a
#' scimeetr object. The results are returned in a list of data frame. The
#' metrics in the table are: title-words frequency, title-words relative
#' frequency, title-words relevance.
#' 
#' @seealso \code{\link{characterize_jo}} for journal characterization, 
#'   \code{\link{characterize_ab}} for abstract-word characterization, 
#'   \code{\link{characterize_kw}} for keyword characterization, 
#'   \code{\link{characterize_au}} for author characterization, 
#'   \code{\link{characterize_un}} for university characterization, 
#'   \code{\link{characterize_co}} for country characterization
#' @param scimeetr_data An object of class scimeetr.
#' @param lambda A number from 0 to 1. If 0 the relevance score would be equal 
#'   to the relative frequency. If 1 for the relevance score would be equal to
#'   the frequency.
#' @examples 
#' # Example with an object of class scimeetr (see import_wos_files() or 
#' # import_scopus_files()) already in the workspace
#' titleword_list <- characterize_ti(scimeetr_list)
#' # Since this example shows how to load WOS from your system we need to run 
#' # the following line to find the path to the exemple file
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' # Then we can run the actual example
#' example_scimeetr_object <- import_wos_files(files_directory = fpath)
#' characterize_ti(example_scimeetr_object)
#' 
#' @return A list of dataframe. The list length matchs the number of communities
#'   that the scimeetr object contains.
#' @import dplyr
#' @export
characterize_ti <- function(scimeetr_data, lambda = 0.4) {
  hold <- purrr::map(scimeetr_data, function(x) {
    # Size
    community_size <- nrow(x$dfsci)
    # Table of most prolific journals
    tmp_df <- x$ti
    names(tmp_df)[1:3] <- c('title_word', 'frequency', 'pourcent' )
    return(tmp_df)
  })
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    purrr::compact()
  hold_relative <- purrr::map(hold[names(tmp)], function(ti_df, lambda) {
    tst <- ti_df %>%
      mutate(relevance = lambda * log(frequency/sum(frequency,na.rm = T), base = 10) + (1 - lambda) * log((frequency/sum(frequency,na.rm = T))/(Frequency/sum(Frequency,na.rm = T)), base = 10))
    return(tst)
  }, lambda)
  ti_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      ti_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'title_word') %>%
        arrange(desc(relevance)) %>%
        select(title_word, frequency.x, Relative_frequency.y:relevance)
      names(ti_df[[x]]) <- c('title_word',
                             'frequency',
                             'relative_frequency',
                             'relevance')
    } else {
      ti_df[[x]] <- hold[[x]][,1:2]
    }
  }
  names(ti_df) <- names(scimeetr_data)
  return(ti_df)
}
