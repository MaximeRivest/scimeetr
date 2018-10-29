#' Characterize authors found within papers' titles
#' 
#' \code{characterize_au} calculates several author related metrics from a
#' scimeetr object. The results are returned in a list of data frame. The
#' metrics in the table are: title-words frequency, title-words relative
#' frequency, title-words relevance.
#' 
#' @seealso \code{\link{characterize_jo}} for journal characterization, 
#'   \code{\link{characterize_ab}} for abstract-word characterization, 
#'   \code{\link{characterize_kw}} for keyword characterization, 
#'   \code{\link{characterize_ti}} for title characterization, 
#'   \code{\link{characterize_un}} for university characterization, 
#'   \code{\link{characterize_co}} for country characterization
#' @param scimeetr_data An object of class scimeetr.
#' @param lambda A number from 0 to 1. 0 for relative frequency 1 for total 
#'   occurence only
#' @examples 
#' # Example with an object of class scimeetr (see import_wos_files() or 
#' # import_scopus_files()) already in the workspace
#' author_list <- characterize_au(scimeetr_list)
#' # Since this example shows how to load WOS from your system we need to run 
#' # the following line to find the path to the exemple file
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' # Then we can run the actual example
#' example_scimeetr_object <- import_wos_files(files_directory = fpath)
#' characterize_au(example_scimeetr_object)
#' 
#' @return A list of dataframe. The list length matchs the number of communities
#'   that the scimeetr object contains.
#' @import dplyr
#' @export
characterize_au <- function(scimeetr_data, lambda = 0.7) {
  splitted_cr <- split_cr(scimeetr_data)
  splt_cr_freq <- map2(map(scimeetr_data, 'cr'), rep(list(splitted_cr), length(scimeetr_data)), full_join, by = c('ID'))
  hold <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, full_join, by = c('RECID')) %>%
    map(function(dfsci_mod){
      au_list <- strsplit(dfsci_mod$AU, split="; ")
      lengthlist <- map(au_list, length)
      position <- map2(as.list(rep(1, length(au_list))), lengthlist, function(x,y){
        if(y >= 3){
          z <- c(x,3:y, 2)
        } else if(y != 0) {
          z <- c(x:y)
        } else {
          z <- NA
        }
        return(z)
      }) %>%
        unlist()
      # find top 10 cut off
      tc_tresh <- quantile(dfsci_mod$TC, c(0.90), na.rm = T)
      lc_tresh <- quantile(dfsci_mod$Frequency.x, c(0.90), na.rm = T)
      
      # ----------------
      audf <- data.frame('AU' = toupper(unlist(au_list)),
                         'UT' = rep(dfsci_mod$UT, sapply(au_list, length)),
                         'TC' = rep(dfsci_mod$TC, sapply(au_list, length)),
                         'RECID' = rep(dfsci_mod$RECID, sapply(au_list, length)),
                         'PY' = rep(dfsci_mod$PY, sapply(au_list, length)),
                         'LC' = rep(dfsci_mod$Frequency.x, sapply(au_list, length)),
                         'PO' = na.omit(position),
                         'LG' = rep(sapply(au_list, length), sapply(au_list, length)),
                         'tc_tresh' = as.numeric(tc_tresh[1]),
                         'lc_tresh' = as.numeric(lc_tresh[1]),
                         stringsAsFactors=F)
      audf$LC[is.na(audf$LC)] <- 0
      audf1 <- audf %>%
        mutate(worth = suppressWarnings(((1/PO)/(sum(1/c(1:LG))))/((1/1)/(sum(1/c(1:LG))))),
               LCC = LC * worth,
               TCC = TC * worth,
               is_lc_highly_cited = LC > lc_tresh,
               is_tc_highly_cited = TC > tc_tresh
          ) %>%
        group_by(AU) %>%
        mutate(res_n = n()) %>%
        #filter(res_n >= 1) %>%
        mutate(rank_LCC = rank(desc(LCC)),
               HHL = LCC > rank_LCC,
               rank_TCC = rank(desc(TCC)),
               HH = TCC > rank_TCC,
               rank_LC = rank(desc(LC)),
               HL = LC > rank_LC,
               rank_TC = rank(desc(TC)),
               H = TC > rank_TC) %>%
        #filter(!is.na(HL) & !is.na(H)) %>%
        summarise(HHL = sum(HHL, na.rm = T),
                  HH = sum(HH, na.rm = T),
                  HL = sum(HL, na.rm = T),
                  H = sum(H, na.rm = T),
                  Local_cit = sum(LC, na.rm = T),
                  Global_cit = sum(TC, na.rm = T),
                  nb_papers = n(),
                  nb_highly_cited_papers_TC = sum(is_tc_highly_cited),
                  nb_highly_cited_papers_LC = sum(is_lc_highly_cited)) %>%
        #filter(H>=1) %>%
        ungroup() %>%
        mutate(local2global = Local_cit / Global_cit) %>%
        arrange(desc(HHL))
      audf1 <- audf1[!is.na(audf1$AU),]
      audf1$AU <- stringr::str_replace_all(audf1$AU, ',', '')
      first_au <- group_by(dfsci_mod, author) %>%
        summarise(fa_nb_paper_cited = n(),
                  fa_total_cit = sum(Frequency.x, na.rm = T))
      df <- full_join(audf1, first_au, by = c('AU' = 'author'))
      return(df)
    })
  # If it's a sub_community, table of relative frequency 
  tmp <- purrr::map(scimeetr_data, "parent_com") %>%
    purrr::compact()
  hold_relative <- purrr::map2(hold[names(tmp)], hold[as.character(tmp)], function(child, parent, lambda) {
    tst <- left_join(child, parent, by = "AU") %>%
      mutate(relevance_based_on_local_cit = lambda * log(Local_cit.x/sum(Local_cit.x,na.rm = T), base = 10) + (1 - lambda) * log((Local_cit.x/sum(Local_cit.x,na.rm = T))/(Local_cit.y/sum(Local_cit.y,na.rm = T)), base = 10),
             relevance_based_on_nb_papers = lambda * log(nb_papers.x/sum(nb_papers.x,na.rm = T), base = 10) + (1 - lambda) * log((nb_papers.x/sum(nb_papers.x,na.rm = T))/(nb_papers.y/sum(nb_papers.y,na.rm = T)), base = 10)) %>%
      select(AU,starts_with('relevance'))
  }, lambda)
  au_df <- list()
  for(x in 1:length(hold)){
    subh <- hold_relative[[names(hold)[x]]]
    if(!is.null(subh)) {
      au_df[[x]] <- left_join(hold[[names(hold)[x]]], hold_relative[[names(hold)[x]]], 'AU')
    } else {
      au_df[[x]] <- hold[[x]]
    }
  }
  names(au_df) <- names(scimeetr_data)
  return(au_df)
}
