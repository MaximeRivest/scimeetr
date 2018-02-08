#' Characterize bibliometric corpus with journals.
#' 
#' \code{characterize_jo} calculates several journal bibliometrics from a 
#' scimeetr object. The results are returned in a list of data frame. The 
#' metrics in the table are: number of citations, H-index, impact factor, number
#' of different papers that were cited by papers in the scimeetr dataframe,
#' number of papers that are within the community. _rel, _rank and _relevance at
#' the end of a column name refers to the fact that the relativem the rank
#' change or the relevance of the journal were calculated based on the metrics
#' that matches the start of the column name.
#' 
#' @seealso \code{\link{characterize_kw}} for keyword characterization, 
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
#' journals <- characterize_jo(scimeetr_list)
#' # Since this example shows how to load WOS from your system we need to run 
#' # the following line to find the path to the exemple file
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' # Then we can run the actual example
#' example_scimeetr_object <- import_wos_files(files_directory = fpath)
#' characterize_jo(example_scimeetr_object)
#' 
#' @return A list of dataframe. The list length matchs the number of communities
#'   that the scimeetr object contains.
#' @import dplyr
#' @export
characterize_jo <- function(scimeetr_data, lambda = 0.6) {
  splitted_cr <- split_cr(scimeetr_data)
  hold <- purrr::map(scimeetr_data, function(x, splitted_cr) {
    # Size
    community_size <- nrow(x$dfsci)
    # Table of most prolific journals
    prolificity_jo <- group_by(x$dfsci, "journal" = J9) %>%
      summarise(papers_within_com = n()) %>%
      arrange(desc(papers_within_com)) %>%
      ungroup()
    # Table of most cited Journals,h index, impact factor, contains most papers that are cited
    citation_jo <- left_join(x$cr, splitted_cr, by = "ID") %>%
      group_by(journal) %>%
      arrange(Frequency.x) %>%
      mutate(rank = rank(desc(Frequency.x)),
             H = Frequency.x > rank) %>%
      filter(!is.na(H)) %>%
      summarise(citations = sum(Frequency.x),
                H = sum(H),
                impact_factor = if_else(n() >= ceiling(community_size/300), mean(Frequency.x), 0),
                papers_cited = n()) %>%
      ungroup()
    tmp <- full_join(citation_jo, prolificity_jo, by = "journal") %>%
      filter(!is.na(journal) & journal != "") %>%
      arrange(desc(H))
    return(tmp)
  }, splitted_cr)
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    purrr::compact()
  hold_relative <- purrr::map2(hold[names(tmp)], hold[as.character(tmp)], function(child, parent, lambda) {
    tst <- left_join(child, parent, by = "journal") %>%
      mutate(citations_rel = (citations.x / citations.y) / (sum(citations.x,na.rm = T)/sum(citations.y, na.rm = T)),
             H_rel = rank(desc(H.y)) - rank(desc(H.x)),
             papers_cited_rank = rank(desc(papers_cited.y)) - rank(desc(papers_cited.x)),
             papers_cited_rel = (papers_cited.x / papers_cited.y) / (sum(papers_cited.x,na.rm = T)/sum(papers_cited.y, na.rm = T)),
             papers_cited_relevance = lambda * log(papers_cited.x/sum(papers_cited.x,na.rm = T), base = 10) + (1 - lambda) * log((papers_cited.x/sum(papers_cited.x,na.rm = T))/(papers_cited.y/sum(papers_cited.y,na.rm = T)), base = 10),
             papers_within_com_rank = rank(desc(papers_within_com.y)) - rank(desc(papers_within_com.x)),
             papers_within_com_rel = (papers_within_com.x / papers_within_com.y) / (sum(papers_within_com.x,na.rm = T)/sum(papers_within_com.y, na.rm = T)),
             papers_within_relevance = lambda * log(papers_within_com.x/sum(papers_within_com.x,na.rm = T), base = 10) + (1 - lambda) * log((papers_within_com.x/sum(papers_within_com.x,na.rm = T))/(papers_within_com.y/sum(papers_within_com.y,na.rm = T)), base = 10)) %>%
      select(journal, citations_rel:papers_within_com_rel)
  }, lambda)
  jo_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      jo_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'journal')
    } else {
      jo_df[[x]] <- hold[[x]]
    }
  }
  names(jo_df) <- names(scimeetr_data)
  return(jo_df)
}
