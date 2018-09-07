#' Make all reading lists
#'
#' @param scimeetr_data A scimeetr object. This is created from importing files
#'   with import_wos_file or scimap().
#' @param length_list An integer that will multiply the standard list length.
#' @return a list of reading list of the same length as the number of
#'   communities found in the input scimeetr object
#' @export
#' @import dplyr purrr
scilist_all <- function(scimeetr_data, length_list = 1, verbose = T, except_bet_clo = T) {
  lev_com <- stringr::str_count(names(scimeetr_data), '_')
  if(verbose == T) {
    cat("Split references \n")
  }
  splt_cr <- split_cr(scimeetr_data)
  splt_cr_freq <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID'))

  if(!any(names(scimeetr_data$com1) == 'graph')) {
    scimeetr_data$com1$graph <- scimeetr:::coupling(scimeetr_data$com1$dfsci)
  }

  scimeetr_data <- map(scimeetr_data, function(com_i, par_com = scimeetr_data$com1){
    if(!any(names(com_i) == 'graph')){
      g <-  par_com$graph
      g1 <- igraph::induced_subgraph(g, igraph::V(g)[com_i$dfsci$UT])
      com_i <- com_i
      com_i$graph <- g1
    } else {
      com_i <- com_i
    }
    return(com_i)
  }
  )

  #---- core_papers ------------------------------------------------------------
  if(verbose == T) {
    cat("core_papers  \n")
  }
  reading_list <- 'core_papers'
  k = ceiling(10 * length_list)
  c1 <- map(scimeetr_data, 'cr') %>%
    map(function(x, k){
      if(any(names(x)=='Relative_frequency')){
        x <- select(x[1:k,], ID:Pourcentage, Relative_frequency)
      } else {
        x <- select(x[1:k,], ID:Pourcentage)
      }
      x$ID <- stringr::str_replace_all(x$ID, ', DOI.*', '')
      x$ID <- stringr::str_replace_all(x$ID, 'V(?=[0-9]{1,6})', '')
      x$ID <- stringr::str_replace_all(x$ID, 'P(?=[0-9]{1,6})', '')
      x <- x[,1:2]
      names(x) <- c("publication", "metric")
      x$list_type <- 'core_papers'
      return(x)
    }, k)
  c1 <- map(c1, as.data.frame, stringsAsFactors = F)
  #---- core_yr ----------------------------------------------------------------
  reading_list <- "core_yr"
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(1*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  c2 <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID')) %>%
    map(function(x, k) {
      x <- x %>%
        mutate(age=as.integer(stringr::str_extract(Sys.Date(), '^[0-9]{4}')) - as.integer(as.character(x$year))) %>%
        filter(age <= 6 & age > 1) %>%
        group_by(age) %>%
        top_n(n = k, wt = Frequency.x) %>%
        select(record, Frequency.x, age) %>%
        arrange(age, desc(Frequency.x)) %>%
        ungroup()
      x$record <- stringr::str_replace_all(x$record, ', DOI.*', '')
      x$record  <- stringr::str_replace_all(x$record , 'V(?=[0-9]{1,6})', '')
      x$record  <- stringr::str_replace_all(x$record , 'P(?=[0-9]{1,6})', '')
      x <- x[,1:2]
      names(x) <- c("publication", "metric")
      x$list_type <- 'core_yr'
      return(x)}, k)
  names(c2) <- names(scimeetr_data)
  c2 <- map(c2, as.data.frame, stringsAsFactors = F)
  #---- core_residual ----------------------------------------------------------
  reading_list = 'core_residual'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  c3 <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID')) %>%
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
      x$res <- res
      x <- x %>%
        top_n(n = k, wt = res) %>%
        arrange(desc(res))
      x$record <- stringr::str_replace_all(x$record, ', DOI.*', '')
      x$record  <- stringr::str_replace_all(x$record , 'V(?=[0-9]{1,6})', '')
      x$record  <- stringr::str_replace_all(x$record , 'P(?=[0-9]{1,6})', '')
      x <- x[,c(1,4)]
      names(x) <- c("publication", "metric")
      x$list_type <- 'core_residual'
      return(x)}, k)
  names(c3) <- names(scimeetr_data)
  c3 <- map(c3, as.data.frame, stringsAsFactors = F)
  #---- by_expert_LC ----------------------------------------------------------
  reading_list = 'by_expert_LC'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(5*length_list)
  m = 2
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  #splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
  bex_lc <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
      audf1 <- audf1 %>%
        mutate(metric = paste(AU,'  h-index :', HL)) %>%
        ungroup() %>%
        select(PAPER:metric)
      names(audf1) <- c('publication', 'metric')
      audf1$list_type <- 'by_expert_LC'
      return(audf1)
    }, k, m)
  names(bex_lc) <- names(scimeetr_data)
  bex_lc <- map(bex_lc, as.data.frame, stringsAsFactors = F)
  #---- by_expert_TC ----------------------------------------------------------
  reading_list = 'by_expert_TC'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(5*length_list)
  m = 2
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  #splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
  bex_tc <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
      audf1 <- audf1 %>%
        mutate(metric = paste(AU,'  h-index :', HL)) %>%
        ungroup() %>%
        select(PAPER:metric)
      names(audf1) <- c('publication', 'metric')
      audf1$list_type <- 'by_expert_TC'
      return(audf1)
    }, k, m)
  names(bex_tc) <- names(scimeetr_data)
  bex_tc <- map(bex_tc, as.data.frame, stringsAsFactors = F)
  #---- group_of_experts_LC ----------------------------------------------------
  reading_list = 'group_of_experts_LC'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  #splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
  gex_lc <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
        summarise(AuS = sum(HL/1:length(HL))) %>% #could change the mean for sum...
        top_n(n = k, wt = AuS) %>%
        arrange(desc(AuS))
      names(audf1) <- c('publication', 'metric')
      audf1$list_type <- 'group_of_experts_LC'
      return(audf1)
    }, k)
  names(gex_lc) <- names(scimeetr_data)
  gex_lc <- map(gex_lc, as.data.frame, stringsAsFactors = F)
  #---- group_of_experts_TC ----------------------------------------------------
  reading_list = 'group_of_experts_TC'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  #splt_cr_freq <- map2(rep(list(splt_cr), length(lsci)), map(lsci, 'cr'), inner_join, by = c('ID'))
  gex_tc <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
        summarise(AuS = sum(HL/1:length(HL))) %>%
        top_n(n = k, wt = AuS) %>%
        arrange(desc(AuS))
      names(audf1) <- c('publication', 'metric')
      audf1$list_type <- 'group_of_experts_TC'
      return(audf1)
    }, k)
  names(gex_tc) <- names(scimeetr_data)
  gex_tc <- map(gex_tc, as.data.frame, stringsAsFactors = F)
  #---- cite_most_others -------------------------------------------------------
  reading_list = 'cite_most_others'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(6*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  co1 <- map(scimeetr_data, 'dfsci') %>%
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
      rl <- rl[1:k,c(1,3)]
      names(rl) <- c('publication', 'metric')
      rl$list_type <- 'cite_most_others'
      return(rl)
    }, splt_cr, k)
  co1 <- map(co1, as.data.frame, stringsAsFactors = F)
  #---- direct_cite_eigen ------------------------------------------------------
  reading_list = 'direct_cite_eigen'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(8*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  co2 <- map(scimeetr_data, 'dfsci') %>%
    map(function(dfsci, splt_cr, k) {
      cr_list <- strsplit(dfsci$CR, split="; ")
      cr_df <- data.frame('RECID' = rep(dfsci$RECID, sapply(cr_list, length)),
                          'DOI' = rep(dfsci$DI, sapply(cr_list, length)),
                          'CR' = toupper(unlist(cr_list)),
                          stringsAsFactors = F)
      cit_graph <- inner_join(cr_df, splt_cr, by = c('CR' = 'record')) %>%
        inner_join(dfsci, by = c('RECID.y' = 'RECID')) %>%
        select('RECID.x', 'RECID.y')
      names(cit_graph) <- c('from', 'to')
      cit_graph <- igraph::graph_from_data_frame(cit_graph, directed = T)
      centr <- igraph::eigen_centrality(cit_graph)
      rl <- data.frame('paper' = names(sort(centr$vector, decreasing = T)[1:k]),
                       'eigen_centrality' = as.vector(sort(centr$vector, decreasing = T)[1:k]))
      names(rl) <- c('publication', 'metric')
      rl$list_type <- 'direct_cite_eigen'
      return(rl)
    }, splt_cr, k)
  co2 <- map(co2, as.data.frame, stringsAsFactors = F)
  #---- betweeness -------------------------------------------------------------
  if(except_bet_clo == F) {
    reading_list = 'betweeness'
    if(verbose == T) {
      cat(paste0(reading_list, ' \n'))
    }
    k = ceiling(4*length_list)
    co3 <- map(scimeetr_data, function(com_i, k){
      if(nrow(com_i$dfsci) >= 1000) {
        igraph::E(com_i$graph)$weight <- max(igraph::E(com_i$graph)$weight) - (igraph::E(com_i$graph)$weight) + 0.0000001
        com_i$graph <- igraph::delete.edges(com_i$graph,
                                            which(
                                              igraph::E(com_i$graph)$weight >= 
                                                quantile(igraph::E(com_i$graph)$weight,
                                                         probs = c(0.25))))
        #com_i$graph <- igraph::delete.vertices(com_i$graph,which(igraph::degree(com_i$graph)<3))
        bet <- igraph::estimate_betweenness(
          com_i$graph,
          directed = F,
          cutoff = 3)
      } else {
        igraph::E(com_i$graph)$weight <- max(igraph::E(com_i$graph)$weight) - (igraph::E(com_i$graph)$weight) + 0.0000001
        com_i$graph <- igraph::delete.edges(com_i$graph,
                                            which(igraph::E(com_i$graph)$weight >= quantile(igraph::E(com_i$graph)$weight, probs = c(0.25))))
        #com_i$graph <- igraph::delete.vertices(com_i$graph,which(igraph::degree(com_i$graph)<3))
        bet <- igraph::betweenness(
          com_i$graph,
          directed = F,
          normalized = T)
      }
      publication <- unique(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])[1:k]
      metric <- sort(bet, decreasing = T)[1:k]
      x <- data.frame(publication, metric, row.names = NULL)
      x$list_type <- 'betweeness'
      return(x)
    }, k)
    names(co3) <-  names(scimeetr_data)
    co3 <- map(co3, as.data.frame, stringsAsFactors = F)
  } else {
    co3 <- NULL
    cat("Betweeness and closeness is not included by default because it requires a lot of computation \n
        Set 'except_bet_clo = F' to change this")
  }

  #---- closeness --------------------------------------------------------------
  if(except_bet_clo == F) {
    reading_list = 'closeness'
    if(verbose == T) {
      cat(paste0(reading_list, ' \n'))
    }
    k = ceiling(4*length_list)
    co4 <- map(scimeetr_data, function(com_i, k){
      if(nrow(com_i$dfsci) >= 1000) {
        bet <- igraph::estimate_closeness(com_i$graph, normalized = T, cutoff = 3)
      } else {
        bet <- igraph::closeness(com_i$graph, normalized = T)
      }
      publication <- unique(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])[1:k]
      metric <- sort(bet, decreasing = T)[1:k]
      x <- data.frame(publication, metric, row.names = NULL)
      x$list_type <- 'closeness'
      return(x)
    }, k)
    names(co4) <-  names(scimeetr_data)
    co4 <- map(co4, as.data.frame, stringsAsFactors = F)
  } else {
    co4 <- NULL
  }
  #---- connectness ------------------------------------------------------------
  reading_list = 'connectness'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  co5 <- map(scimeetr_data, function(com_i, k){
    bet <- igraph::degree(com_i$graph, normalized = F)
    publication <- unique(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])
    metric <- sort(bet, decreasing = T)[1:k]
    x <- data.frame(publication, metric, row.names = NULL)
    x$list_type <- 'connectness'
    return(x)
  }, k)
  names(co5) <-  names(scimeetr_data)
  co5 <- map(co5, as.data.frame, stringsAsFactors = F)
  #---- link_strength ----------------------------------------------------------
  reading_list = 'link_strength'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  co6 <- map(scimeetr_data, function(com_i, k){
    bet <- igraph::strength(com_i$graph, mode = 'all', weights = igraph::E(com_i$graph)$weight)
    publication <- unique(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID'])
    metric <- sort(bet, decreasing = T)[1:k]
    x <- data.frame(publication, metric, row.names = NULL)
    x$list_type <- 'link_strength'
    return(x)
  }, k)
  names(co6) <-  names(scimeetr_data)
  co6 <- map(co6, as.data.frame, stringsAsFactors = F)
  #---- page_rank --------------------------------------------------------------
  reading_list = 'page_rank'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  co7 <- map(scimeetr_data, function(com_i, k){
    bet <- igraph::page_rank(com_i$graph, directed = F)
    publication <- unique(com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet$vector, decreasing = T)[1:k]), 'RECID'])
    metric <- sort(bet$vector, decreasing = T)[1:k]
    x <- data.frame(publication, metric, row.names = NULL)
    x$list_type <- 'page_rank'
    return(x)
  }, k)
  names(co7) <-  names(scimeetr_data)
  co7 <- map(co7, as.data.frame, stringsAsFactors = F)
  #---- journal_dis ------------------------------------------------------------
  reading_list == 'journal_dis'
  if(verbose == T) {
    cat(paste0(reading_list, ' \n'))
  }
  k = ceiling(4*length_list)
  #lev_com <- stringr::str_count(names(lsci), '_')
  #splt_cr <- split_cr(lsci)
  jo_dis <- map(scimeetr_data, 'dfsci') %>%
    map(function(dfsci, splt_cr, k) {
      cr_list <- strsplit(dfsci$CR, split="; ")
      cr_df <- data.frame('RECID' = rep(dfsci$RECID, sapply(cr_list, length)),
                          'DOI' = rep(dfsci$DI, sapply(cr_list, length)),
                          'CR' = toupper(unlist(cr_list)),
                          stringsAsFactors = F)
      rl <- inner_join(cr_df, splt_cr, by = c('CR' = 'record')) %>%
        group_by(RECID.x) %>%
        summarise(metric = length(unique(journal)),
                  norm_metric = length(unique(journal))/n()) %>%
        arrange(desc(metric))
      rl <- rl[1:k,] # other lists kept only publication and metric
      names(rl) <- c('publication', 'metric')
      rl$list_type <- 'number_of_differente_journal'
      return(rl)
    }, splt_cr, k)
  jo_dis <- map(jo_dis, as.data.frame, stringsAsFactors = F)
  #---- Combine lists ----------------------------------------------------------
  rl <- list(c1,c2,c3,bex_lc,bex_tc,gex_lc,gex_tc,co1,co2,co3,
             co4,co5,co6,co7,jo_dis)
  rl <- discard(rl, is_null)
  rl <- purrr::transpose(rl)
  rl <- purrr::map(rl, function(x) {
    purrr::map(x, function(y){
      y$metric <- as.character(y$metric)
      y$publication <- as.character(y$publication)
      y$list_type <- as.character(y$list_type)
      return(y)})})
  rl <- purrr::map(rl, dplyr::bind_rows)
  return(rl)
}
