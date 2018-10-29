#' Characterize bibliometric corpus with keywords.
#' 
#' \code{characterize_kw} calculates several keyword metrics from a scimeetr 
#' object. The results are returned in a list of data frame. The metrics in the 
#' table are: keywords frequency, keywords relative frequency, and keywords 
#' relevance.
#' 
#' @seealso \code{\link{characterize_jo}} for journal characterization, 
#'   \code{\link{characterize_ti}} for title-word characterization, 
#'   \code{\link{characterize_ab}} for abstract-word characterization, 
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
#' keywords_list <- characterize_kw(scimeetr_list)
#' # Since this example shows how to load WOS from your system we need to run 
#' # the following line to find the path to the exemple file
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' # Then we can run the actual example
#' example_scimeetr_object <- import_wos_files(files_directory = fpath)
#' characterize_kw(example_scimeetr_object)
#' 
#' @return A list of dataframe. The list length matchs the number of communities
#'   that the scimeetr object contains.
#' @import dplyr
#' @export
characterize_kw <- function(scimeetr_data, lambda = 0.4) {
  hold <- purrr::map(scimeetr_data, function(x) {
    # Size
    community_size <- nrow(x$dfsci)
    # Table of most prolific journals
    id <- toupper(unlist(stringr::str_split(x$dfsci$ID, '; ')))
    de <- toupper(unlist(stringr::str_split(x$dfsci$DE, '; ')))
    idnde <- c(id, de)
    iddf <- as.data.frame(table(id), stringsAsFactors = F) %>%
      filter(Freq > 3 & 0 != nchar(id)) 
    dedf <- as.data.frame(table(de), stringsAsFactors = F) %>%
      filter(Freq > 3 & 0 != nchar(de))
    idndedf <- as.data.frame(table(idnde), stringsAsFactors = F) %>%
      filter(Freq > 3 & 0 != nchar(idnde))
    tmp_df <- left_join(idndedf, dedf, by = c('idnde' = 'de'), suffix = c("_id_and_de", "_de")) %>%
      left_join(iddf, by = c("idnde" = "id")) %>%
      arrange(desc(Freq_de))
    names(tmp_df) <- c('keyword', 'id_and_de_frequency', 'de_frequency', 'id_frequency' )
    return(tmp_df)
  })
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    purrr::compact()
  hold_relative <- purrr::map2(hold[names(tmp)], hold[as.character(tmp)], function(child, parent, lambda) {
    tst <- left_join(child, parent, by = "keyword") %>%
      mutate(id_and_de_relative = (id_and_de_frequency.x / id_and_de_frequency.y) / (sum(id_and_de_frequency.x,na.rm = T)/sum(id_and_de_frequency.y, na.rm = T)),
             de_relative = (de_frequency.x / de_frequency.y) / (sum(de_frequency.x,na.rm = T)/sum(de_frequency.y, na.rm = T)),
             id_relative = (id_frequency.x / id_frequency.y) / (sum(id_frequency.x,na.rm = T)/sum(id_frequency.y, na.rm = T)),
             id_and_de_relevancy = lambda * log(id_and_de_frequency.x/sum(id_and_de_frequency.x,na.rm = T), base = 10) + (1 - lambda) * log((id_and_de_frequency.x/sum(id_and_de_frequency.x,na.rm = T))/(id_and_de_frequency.y/sum(id_and_de_frequency.y,na.rm = T)), base = 10),
             de_relevance = lambda * log(de_frequency.x/sum(de_frequency.x,na.rm = T), base = 10) + (1 - lambda) * log((de_frequency.x/sum(de_frequency.x,na.rm = T))/(de_frequency.y/sum(de_frequency.y,na.rm = T)), base = 10),
             id_relevance = lambda * log(id_frequency.x/sum(id_frequency.x,na.rm = T), base = 10) + (1 - lambda) * log((id_frequency.x/sum(id_frequency.x,na.rm = T))/(id_frequency.y/sum(id_frequency.y,na.rm = T)), base = 10)) %>%
      select(keyword, id_and_de_relative:id_relevance)
    return(tst)
  }, lambda)
  kw_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      kw_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'keyword') %>%
        arrange(desc(de_relevance))
    } else {
      kw_df[[x]] <- hold[[x]]
    }
  }
  names(kw_df) <- names(scimeetr_data)
  return(kw_df)
}
