#' Make a list of dataframe of Authors frequencies
#' 
#' This can be used to characterize the communities of research.
#'
#' @param scimeetr_data A scimeetr object
#' @return A list of dataframe containing Author related metrics
#' @import dplyr
#' @export
characterize_au <- function(scimeetr_data) {
  splitted_cr <- split_cr(scimeetr_data)
  splt_cr_freq <- map2(map(scimeetr_data, 'cr'), rep(list(splitted_cr), length(lsci)), full_join, by = c('ID'))
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
      audf <- data.frame('AU' = toupper(unlist(au_list)),
                         'UT' = rep(dfsci_mod$UT, sapply(au_list, length)),
                         'TC' = rep(dfsci_mod$TC, sapply(au_list, length)),
                         'RECID' = rep(dfsci_mod$RECID, sapply(au_list, length)),
                         'PY' = rep(dfsci_mod$PY, sapply(au_list, length)),
                         'LC' = rep(dfsci_mod$Frequency.x, sapply(au_list, length)),
                         'PO' = na.omit(position),
                         'LG' = rep(sapply(au_list, length), sapply(au_list, length)),
                         stringsAsFactors=F)
      audf1 <- audf %>%
        mutate(worth = suppressWarnings(((1/1)/(sum(1/c(1:LG))))/((1/PO)/(sum(1/c(1:LG))))),
               LCC = LC * worth,
               TCC = TC * worth) %>%
        group_by(AU) %>%
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
                  nb_papers = n()) %>%
        ungroup() %>%
        mutate(local2global = Local_cit / Global_cit) %>%
        arrange(desc(HHL))
      audf1 <- na.omit(audf1)
      audf1$AU <- stringr::str_replace_all(audf1$AU, ',', '')
      first_au <- group_by(dfsci_mod, author) %>%
        summarise(fa_nb_paper_cited = n(),
                  fa_total_cit = sum(Frequency.x, na.rm = T))
      df <- full_join(audf1, first_au, by = c('AU' = 'author'))
      return(df)
    })
  return(hold)
}
