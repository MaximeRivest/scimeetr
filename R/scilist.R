#' Make reading lists
#'
#' \code{scilist} ranks papers according to different metrics. The metric is
#' determined by the argument 'reading_list'.
#'
#' With the parameter reading_list, we can get any of the following 12 reading
#' lists that fits into three categories: \itemize{ \item Core \itemize{ \item
#' core_papers \item core_yr \item core_residual } \item Experts \itemize{ \item
#' by_expert_LC \item by_expert_TC \item group_of_experts_TC \item
#' group_of_experts_LC } \item Centrality \itemize{ \item cite_most_others \item
#' betweeness \item closeness \item connectness \item page_rank } }
#'
#' Using \code{scilist} with reading_list = "core_papers" will list the most
#' cited paper. The parameter k controls the number of paper to list.
#'
#' Using \code{scilist} with reading_list = "core_yr" will list the most cited
#' paper for each year from three years before present to ten years before
#' present. The parameter k controls the number of paper per year to list.
#'
#' Using \code{scilist} with reading_list = "core_residual" will list the papers
#' that diverge most from the expected number of citation for this particular
#' paper.
#'
#' Using \code{scilist} with reading_list = "by_expert_LC" we will get a list of
#' recent papers by one or a few experts in the community. For the option
#' by_expert_LC, authors are ranked based on their harmonic local H-index. The
#' H-index is a measure of an other productivity and impact. An author with an
#' H-index of 10 means that he has published at least 10 papers with 10 or more
#' citation each. A local H-index means that only citations from other papers in
#' the community are counted. A harmonic local H-index means that authors do not
#' get the full credit for each citation their paper received. It is corrected
#' depending on the authos position in the authors list. First authors gets most
#' of the credit, then the last author gets the second most, and the authors
#' gets credit as a proportion of their position. Once the authors
#' harmonic-local-H-index is found they are ranked and the m most recent
#' publication of the k most 'expert' authors are listed as a reading list.
#'
#' Using \code{scilist} with reading_list = "by_expert_TC" instead of
#' reading_list = "by_expert_LC", notice the _TC instead of the _LC will based
#' the ranking calculation on total citation of it's publications instead of
#' only the local citations.
#'
#' Using \code{scilist} with reading_list = "group_of_experts_LC" we will get a
#' list of papers for which many authors are experts in the community. For this
#' option, authors are assigned a harmonic local H-index like described in the
#' previous section. But this time, a weighted sum of the harmonic-local-H-index
#' of each authors of a paper is calculated.
#'
#' Betweeness measures the importance of a paper in connecting two clusters of
#' papers. Papers with a high betweeness would therefore be a paper that tend to
#' be more interdisciplinary.
#'
#' Closeness measures the average number of link between a paper and all other
#' papers. Papers with a high closeness would therefore be a paper that tend to
#' have a large and wide list of citations.
#'
#' Connectness measures the number of links a paper has. Papers with a high
#' connectness would therefore be a paper that tend to have cited what most
#' other studies cited.
#'
#' With the option cite_most_others, the papers that cite most other papers of
#' the community can be found. This is not a centrality measure but it is also
#' based on papers connection to each other. It should tend to find litterature
#' review and recent papers that have an especially good grasp on the community.
#'
#' @param scimeetr_data An object of class '\link{scimeetr}'
#' @param reading_list A vector of length one. Equal to either: 'core_papers',
#'   'core_yr', 'core_residual', 'by_expert_LC', 'by_expert_TC',
#'   'group_of_experts_TC', 'group_of_experts_LC', 'cite_most_others',
#'   'direct_cite_eigen', 'betweeness', 'closeness', 'connectness',
#'   'link_strength', 'page_rank', 'journal_dis', 'journal_unique_combn'
#' @param k Length of list per community
#' @param m Not always used. \code{m} is used when there is a two step filter.
#'   An example of a two step filter is the list generated when reading_list =
#'   'by_expert'. First k will determine how many experts should be listed, then
#'   m will determine how many papers per experts should be listed.
#' @return A data frame.
#' @export
#' @import dplyr purrr
scilist <- function(scimeetr_data, reading_list = 'core_papers', k = 5, m = 3) {
  if(reading_list == 'core_papers') {
    rl <- map(scimeetr_data, 'cr') %>%
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

  } else if (reading_list == 'core_yr') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    rl <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID')) %>%
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
    names(rl) <- names(scimeetr_data)
  } else if (reading_list == 'core_residual') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    rl <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID')) %>%
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
        x$record <- stringr::str_replace_all(x$record, ', DOI.*', '')
        x$record  <- stringr::str_replace_all(x$record , 'V(?=[0-9]{1,6})', '')
        x$record  <- stringr::str_replace_all(x$record , 'P(?=[0-9]{1,6})', '')
        x <- x[,1:2]
        names(x) <- c("publication", "metric")
        x$list_type <- 'core_residual'
        return(x)}, k)
    names(rl) <- names(scimeetr_data)
  } else if (reading_list == 'by_expert_LC') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    splt_cr_freq <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
    names(rl) <- names(scimeetr_data)
  } else if (reading_list == 'by_expert_TC') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    splt_cr_freq <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
    names(rl) <- names(scimeetr_data)
  } else if (reading_list == 'group_of_experts_LC') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    splt_cr_freq <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
    names(rl) <- names(scimeetr_data)
  } else if (reading_list == 'group_of_experts_TC') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    splt_cr_freq <- map2(rep(list(splt_cr), length(scimeetr_data)), map(scimeetr_data, 'cr'), inner_join, by = c('ID'))
    rl <- map2(map(scimeetr_data, 'dfsci'), splt_cr_freq, left_join, by = c('RECID')) %>%
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
    names(rl) <- names(scimeetr_data)
  } else if (reading_list == 'cite_most_others') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    rl <- map(scimeetr_data, 'dfsci') %>%
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
  } else if (reading_list == 'direct_cite_eigen') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    rl <- map(scimeetr_data, 'dfsci') %>%
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
  } else if (reading_list == 'betweeness') {
    rl <- map(scimeetr_data, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      igraph::E(com_i$graph)$weight <- max(igraph::E(com_i$graph)$weight) - (igraph::E(com_i$graph)$weight) + 0.0000001
      com_i$graph <- igraph::delete.edges(com_i$graph,
                                 which(igraph::E(com_i$graph)$weight >= quantile(igraph::E(com_i$graph)$weight, probs = c(0.25))))
      #com_i$graph <- igraph::delete.vertices(com_i$graph,which(igraph::degree(com_i$graph)<3))
      bet <- igraph::betweenness(com_i$graph, directed = F, normalized = T)
      bet_df <- data.frame(UT = names(bet), bet = bet, stringsAsFactors = F)
      x <- left_join(bet_df, com_i$dfsci, by = 'UT') %>%
        arrange(desc(bet)) %>%
        mutate(ranks = 1:n()) %>%
        filter(ranks <= k) %>%
        select('publication' = 'RECID',
               'metric' = 'bet')
      x$list_type <- 'betweeness'
      return(x)
    }, k)
    names(rl) <-  names(scimeetr_data)
  } else if (reading_list == 'closeness') {
    rl <- map(scimeetr_data, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::closeness(com_i$graph, normalized = T)
      publication <- com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID']
      metric <- sort(bet, decreasing = T)[1:k]
      x <- data.frame(publication, metric, row.names = NULL)
      x$list_type <- 'closeness'
      return(x)
    }, k)
    names(rl) <-  names(scimeetr_data)
  } else if (reading_list == 'connectness') {
    rl <- map(scimeetr_data, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::degree(com_i$graph, normalized = F)
      publication <- com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID']
      metric <- sort(bet, decreasing = T)[1:k]
      x <- data.frame(publication, metric, row.names = NULL)
      x$list_type <- 'connectness'
      return(x)
    }, k)
    names(rl) <-  names(scimeetr_data)
  } else if (reading_list == 'link_strength') {
    rl <- map(scimeetr_data, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::strength(com_i$graph, mode = 'all', weights = igraph::E(com_i$graph)$weight)
      publication <- com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet, decreasing = T)[1:k]), 'RECID']
      metric <- sort(bet, decreasing = T)[1:k]
      x <- data.frame(publication, metric, row.names = NULL)
      x$list_type <- 'link_strength'
      return(x)
    }, k)
    names(rl) <-  names(scimeetr_data)
  } else if (reading_list == 'page_rank') {
    rl <- map(scimeetr_data, function(com_i, k){
      if(!any(names(com_i) == 'graph')){
        com_i$graph <- coupling(com_i$dfsci, 'bic')
      }
      bet <- igraph::page_rank(com_i$graph, directed = F)
      publication <- com_i$dfsci[com_i$dfsci$UT %in% names(sort(bet$vector, decreasing = T)[1:k]), 'RECID']
      metric <- sort(bet$vector, decreasing = T)[1:k]
      x <- data.frame(publication, metric, row.names = NULL)
      x$list_type <- 'page_rank'
      return(x)
    }, k)
    names(rl) <-  names(scimeetr_data)
  } else if (reading_list == 'journal_dis') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    rl <- map(scimeetr_data, 'dfsci') %>%
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
        names(rl) <- c('publication', 'metric', 'weighted_metric')
        rl$list_type <- 'number_of_differente_journal'
        return(rl)
      }, splt_cr, k)
  } else if (reading_list == 'journal_unique_combn') {
    lev_com <- stringr::str_count(names(scimeetr_data), '_')
    splt_cr <- split_cr(scimeetr_data)
    rl <- map(scimeetr_data, 'dfsci') %>%
      map(function(dfsci, splt_cr, k) {
        cr_list <- strsplit(dfsci$CR, split="; ")
        cr_df <- data.frame('RECID' = rep(dfsci$RECID, sapply(cr_list, length)),
                            'DOI' = rep(dfsci$DI, sapply(cr_list, length)),
                            'CR' = toupper(unlist(cr_list)),
                            stringsAsFactors = F)
        rl <- inner_join(cr_df, splt_cr, by = c('CR' = 'record')) %>%
          group_by(journal) %>%
          mutate(n = n()) %>%
          ungroup() %>%
          filter(n > 3)

        rec2jo <- split(rl$journal,rl$RECID.x)
        jo_list <- purrr::map(rec2jo, function(.x) {
          if(length(unique(.x)) >=2){
            purrr::map(combn(unique(.x), m = 2, simplify = F), paste, collapse = '_')
          } else {
            NULL
          }
        })
        jo_df <- data.frame('RECID' = rep(names(jo_list), sapply(jo_list, length)),
                            'JO_combn' = toupper(unlist(jo_list)),
                            stringsAsFactors = F)
        ndf <- nrow(dfsci)
        jo_df <- jo_df %>%
          group_by(JO_combn) %>%
          mutate(freq = n()) %>%
          ungroup() %>%
          mutate(freq = max(freq)) %>%
          group_by(JO_combn) %>%
          mutate(freq = (1-n()/freq)^2) %>%
          group_by(RECID) %>%
          summarise(metric = sum(freq)) %>%
          arrange(desc(metric))
        rl <- jo_df[1:k,] # other lists kept only publication and metric
        names(rl) <- c('publication', 'metric')
        rl$list_type <- 'journal_unique_combn'
        return(rl)
      }, splt_cr, k)
  }
  return(map(rl, as.data.frame))
}




