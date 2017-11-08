#' Make reading lists
#' 
#' @param biblio_df A data.frame of class scimeetr created from importing files
#' with import_bib_file or a list of class scimeetr created from the 
#' function scimap.
#' @param reading_list A vector of length one. Equal to either: 
#' 'core_papers', 'core_yr', 'core_residual', 'by_expert_LC', 'by_expert_TC',
#' 'group_of_experts_TC', 'group_of_experts_LC', 'cite_most_others',
#' 'betweeness', 'closeness', 'connectness', 'page_rank'
#' 
#' @param k length of list per community
#' @return df. A reading list
#' @export
#' @import dplyr purrr
scilist <- function(lsci = lsci, splt_cr = splt_cr, reading_list = 'core_papers', k = 8, m = 3) {
  if(reading_list == 'core_papers') {
    rl <- map(lsci, 'cr') %>%
      map(function(x, k){
        if(any(names(x)=='Relative_frequency')){
          x <- select(x[1:k,], ID:Pourcentage, Relative_frequency)
        } else {
          x <- select(x[1:k,], ID:Pourcentage)
        }
        return(x)
      }, k)
  } else if (reading_list == 'core_yr') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com) 
    rl <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID')) %>%
      map(function(x, k) {
        x <- x %>%
          mutate(age=as.integer(stringr::str_extract(Sys.Date(), '^[0-9]{4}')) - as.integer(as.character(x$year))) %>%
          filter(age <= 10 & age > 2) %>% 
          group_by(age) %>%
          top_n(n = k, wt = Frequency.x) %>%
          select(record, Frequency.x, age) %>%
          arrange(age, desc(Frequency.x)) %>%
          ungroup()
        return(x)}, k) 
    names(rl) <- names(lsci)
  } else if (reading_list == 'core_residual') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com) 
    rl <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID')) %>%
      map(function(x,k) {
        x <- x %>%
          mutate(age=as.integer(stringr::str_extract(Sys.Date(), '^[0-9]{4}')) - as.integer(as.character(x$year))) %>%
          filter(age <= 40 & age > 2) %>% 
          group_by(age) %>%
          top_n(n = 10, wt = Frequency.x) %>%
          select(record, Frequency.x, age) %>%
          arrange(age, desc(Frequency.x)) %>%
          ungroup()
        res <- mgcv::gam(Frequency.x~s(age, k=10), data = x, family = "poisson")$residuals
        x <- x[which(res >= sort(res, decreasing = T)[k]),]
        return(x)}, k)
    names(rl) <- names(lsci)
  } else if (reading_list == 'by_expert_LC') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com) 
    splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(lsci, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
      map(function(dfsci_mod, k, m){
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
        audf <- data.frame('AU' = unlist(au_list), 
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
          mutate(rank = rank(desc(LCC)),
                 HL = LCC > rank) %>%
          filter(!is.na(HL)) %>%
          summarise(HL = sum(HL)) %>%
          ungroup() %>%
          top_n(n = k, wt = HL) %>%
          left_join(audf, by = 'AU') %>%
          left_join(dfsci_mod, by = 'UT') %>%
          group_by(AU.x) %>%
          top_n(n = m, wt = PY.y) %>%
          arrange(desc(HL), AU.x, desc(PY.y)) %>%
          select(AU = AU.x, HL, PAPER = RECID.x)
        return(audf1)
      }, k, m)
    names(rl) <- names(lsci)
  } else if (reading_list == 'by_expert_TC') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com) 
    splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(lsci, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
      map(function(dfsci_mod, k, m){
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
        audf <- data.frame('AU' = unlist(au_list), 
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
          mutate(rank = rank(desc(TCC)),
                 HL = TCC > rank) %>%
          filter(!is.na(HL)) %>%
          summarise(HL = sum(HL)) %>%
          ungroup() %>%
          top_n(n = k, wt = HL) %>%
          left_join(audf, by = 'AU') %>%
          left_join(dfsci_mod, by = 'UT') %>%
          group_by(AU.x) %>%
          top_n(n = m, wt = PY.y) %>%
          arrange(desc(HL), AU.x, desc(PY.y)) %>%
          select(AU = AU.x, HL, PAPER = RECID.x)
        return(audf1)
      }, k, m)
    names(rl) <- names(lsci)
  } else if (reading_list == 'group_of_experts_LC') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com) 
    splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(lsci, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
      map(function(dfsci_mod, k, m){
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
        audf <- data.frame('AU' = unlist(au_list), 
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
          mutate(rank = rank(desc(LCC)),
                 HL = LCC > rank) %>%
          filter(!is.na(HL)) %>%
          summarise(HL = sum(HL)) %>%
          right_join(audf, by = 'AU') %>%
          filter(!is.na(HL)) %>%
          group_by(RECID) %>%
          summarise(AuS = mean(HL)) %>% #could change the mean for sum...
          top_n(n = k, wt = AuS) %>%
          arrange(desc(AuS))
      }, k)
    names(rl) <- names(lsci)
  } else if (reading_list == 'group_of_experts_TC') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com) 
    splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(lsci, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
      map(function(dfsci_mod, k, m){
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
        audf <- data.frame('AU' = unlist(au_list), 
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
          mutate(rank = rank(desc(TCC)),
                 HL = TCC > rank) %>%
          filter(!is.na(HL)) %>%
          summarise(HL = sum(HL)) %>%
          right_join(audf, by = 'AU') %>%
          filter(!is.na(HL)) %>%
          group_by(RECID) %>%
          summarise(AuS = mean(HL)) %>%
          top_n(n = k, wt = AuS) %>%
          arrange(desc(AuS))
      }, k)
    names(rl) <- names(lsci)
  } else if (reading_list == 'cite_most_others') {
    lev_com <- stringr::str_count(names(lsci), '_')
    # parent_com <- map_df(lsci, 'dfsci')
    # parent_com <- unique(parent_com)
    # splt_cr <- split_cr(parent_com)
    rl <- map(lsci, 'dfsci') %>%
      map(function(dfsci, splt_cr, k) {
      cr_list <- strsplit(dfsci$CR, split="; ")
      cr_df <- data.frame('RECID' = rep(dfsci$RECID, sapply(cr_list, length)),
                          'DOI' = rep(dfsci$DI, sapply(cr_list, length)),
                          'CR' = toupper(unlist(cr_list)), 
                          stringsAsFactors = F)
      rl <- inner_join(cr_df, splt_cr, by = c('CR' = 'record')) %>%
        inner_join(dfsci, by = c('RECID.y' = 'RECID')) %>%
        group_by(RECID.x) %>%
        summarise('DOI' = DOI[1],
                  'Nb_of_ref_within_com' = n()) %>%
        arrange(desc(Nb_of_ref_within_com))
      rl <- rl[1:k,]
      return(rl)
    }, splt_cr, k)
  } else if (reading_list == 'betweeness') {
    rl <- map(lsci, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::betweenness(com_i$graph, directed = F, normalized = T)
      return(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])
    }, k)
    names(rl) <-  names(lsci)
  } else if (reading_list == 'closeness') {
    rl <- map(lsci, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::closeness(com_i$graph, normalized = T)
      return(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])
    }, k)
    names(rl) <-  names(lsci)
  } else if (reading_list == 'connectness') {
    rl <- map(lsci, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::degree(com_i$graph, normalized = T)
      return(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])
    }, k)
    names(rl) <-  names(lsci)
  } else if (reading_list == 'page_rank') {
    rl <- map(lsci, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::page_rank(com_i$graph, directed = F)
      return(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet$vector, decreasing = T)[1:k]), 'RECID'])
    }, k)
    names(rl) <-  names(lsci)
  }
  return(map(rl, as.data.frame))
}


