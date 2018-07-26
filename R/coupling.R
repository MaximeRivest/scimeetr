#' Make graph based on coupling
#'
#' @param dfsci A data.frame from a scimeetr object.
#' @param coupling_by A character vector of length one. Equal to either: 'bic' 
#'   for bibliographic coupling, 'kec' for keyword coupling, 'tic' for title 
#'   coupling, 'abc' for abstract coupling, 'joc' for journal coupling, 'auc' 
#'   for author coupling', 'woc' for word coupling, 'bickec' for a combination
#'   of bibliographic coupling and keyword coupling, and 'bickecticjoc' for a
#'   combination of bic, kec, tic, joc (this might be the better way for most
#'   use but it is a bit slow, this is why it is not the default choice)
#' @return graph object
#' @import dplyr stringr data.table

coupling <- function(dfsci = dfsci, coupling_by = 'bic', kw = 1, ti = 1, ab = 1, splitted_cr){
  # Change factors to characters to allow text manipulation
  for(i in 1:ncol(dfsci)){
    if(is.factor(dfsci[,i])){
      dfsci[,i] <- as.character(dfsci[,i])
    }
  }
  # Switch for coupling options
  if(coupling_by == 'bic'){
    cr_list <- strsplit(dfsci$CR, split="; ")
    names(cr_list) <- dfsci$UT
    
    crutdf.x <- na.omit(data.table::data.table(
      'UT.x'= rep(names(cr_list), sapply(cr_list, length)),
      'CR' = unlist(cr_list),
      key = "CR"))
    
    crutdf.y  <- na.omit(data.table::data.table(
      'UT.y'= rep(names(cr_list), sapply(cr_list, length)),
      'CR' = unlist(cr_list),
      key = "CR"))
    
    couple_df <- crutdf.x[crutdf.y, nomatch=0L, on = 'CR', allow.cartesian = T]
    couple_df <- couple_df[UT.x > UT.y]
    couple_df[,`:=`(count = .N), by = .(UT.x, UT.y)]
    couple_df <- couple_df[!duplicated(couple_df[,c(1,3)]), .(UT.x, UT.y, count)]
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               dfsci[,c('UT', 'NR')],
                                               by.x = 'UT.x',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               dfsci[,c('UT', 'NR')],
                                               by.x = 'UT.y',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df[, w_ij := count/sqrt(NR.x * NR.y)]
    couple_df <- couple_df[,.(UT.x, UT.y, w_ij)]
    # couple_df <- inner_join(crutdf, crutdf, by = 'CR') %>%
    #   filter(UT.x > UT.y) %>%
    #   group_by(UT.x, UT.y) %>%
    #   summarise(count = n()) %>%
    #   left_join(dfsci[,c('UT', 'NR')], by = c('UT.x' = 'UT'))%>%
    #   left_join(dfsci[,c('UT', 'NR')], by = c('UT.y' = 'UT'))%>%
    #   mutate(w_ij = count/sqrt(NR.x * NR.y)) %>%
    #   select(UT.x, UT.y, w_ij)
    rm(crutdf.x)
    rm(crutdf.y)
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df <- couple_df[w_ij != 0]
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    rm(coup2)
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
    rm(k_i)
    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                         "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)

  } else if(coupling_by == 'kec'){

    DE_list <- strsplit(dfsci$DE, "[;][ ]")
    names(DE_list) <- dfsci$UT
    deutdf <- na.omit(data.table::data.table(
      'UT'= rep(names(DE_list), sapply(DE_list, length)),
      'KW' =  tolower(unlist(DE_list))))
    
    ID_list <- strsplit(dfsci$ID, "[;][ ]")
    names(ID_list) <- dfsci$UT
    idutdf <- na.omit(data.table::data.table(
      'UT'= rep(names(ID_list), sapply(ID_list, length)),
      'KW' =  tolower(unlist(ID_list))))
    
    kwutdf <- as.data.table(rbind(idutdf, deutdf), key = KW)

    kw_length <- group_by(kwutdf, UT) %>%
      summarize(NK = n()) 
    kw_length <- as.data.table(kw_length)
    
    crutdf.x <- kwutdf
    names(crutdf.x)[1] <- "UT.x"
    setkey(crutdf.x, 'KW')
    crutdf.y  <- kwutdf
    names(crutdf.y)[1] <- "UT.y"
    setkey(crutdf.y, 'KW')
    
    couple_df <- crutdf.x[crutdf.y, nomatch=0L, on = 'KW', allow.cartesian = T]
    couple_df <- couple_df[UT.x > UT.y]
    couple_df[,`:=`(count = .N), by = .(UT.x, UT.y)]
    couple_df <- couple_df[!duplicated(couple_df[,c(1,3)]), .(UT.x, UT.y, count)]
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               kw_length,
                                               by.x = 'UT.x',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               kw_length,
                                               by.x = 'UT.y',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df[, w_ij := count/sqrt(NK.x * NK.y)]
    couple_df <- couple_df[,.(UT.x, UT.y, w_ij)]
    # 
    # DE_list <- strsplit(dfsci$DE, "[;][ ]")
    # names(DE_list) <- dfsci$UT
    # deutdf <- data.frame('UT'= rep(names(DE_list), sapply(DE_list, length)),
    #                      'KW' =  tolower(unlist(DE_list)),
    #                      stringsAsFactors=F)
    # 
    # ID_list <- strsplit(dfsci$ID, "[;][ ]")
    # names(ID_list) <- dfsci$UT
    # idutdf <- data.frame('UT'= rep(names(ID_list), sapply(ID_list, length)),
    #                      'KW' =  tolower(unlist(ID_list)),
    #                      stringsAsFactors=F)
    # 
    # kwutdf <- rbind(idutdf, deutdf)
    # kw_length <- group_by(kwutdf, UT) %>%
    #   summarize(NK = n())
    # couple_df <- inner_join(kwutdf, kwutdf, by = 'KW') %>%
    #   filter(UT.x > UT.y) %>%
    #   group_by(UT.x, UT.y) %>%
    #   summarise(count = n()) %>%
    #   left_join(kw_length, by = c('UT.x' = 'UT'))%>%
    #   left_join(kw_length, by = c('UT.y' = 'UT'))%>%
    #   mutate(w_ij = count/sqrt(NK.x * NK.y)) %>%
    #   select(UT.x, UT.y, w_ij)
    # couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    # couple_df <- filter(couple_df, w_ij != 0)
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))

    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
  } else if(coupling_by == 'tic'){
    documents <- tolower(dfsci$TI)
    documents <- tm::Corpus(tm::VectorSource(documents))
    documents <- tm::tm_map(documents, tm::removePunctuation)
    documents <- tm::tm_map(documents, tm::removeNumbers)
    meaningless_word <- c(tm::stopwords("english"), 'use', 'used', 'using', 'uses',
                          'new', 'effect', 'effects', 'affect', 'affects', 'impact',
                          'impacts', 'implication', 'implications', 'potential',
                          'influence', 'influences', 'influenced', 'study', '-',
                          'data', 'can', 'results', 'different', 'similar', 'also',
                          'c', 'may', 'based', 'important', 'within','however',
                          'found', 'analysis', 'changes', 'among', 'large',
                          'number', 'higher', 'well', 'studies', 'total',
                          'increased', 'increases', 'elsevier', 'level', 'many',
                          'rights', 'present', 'will', 'low', 'across', 'showed',
                          'associated', 'approach', 'related', 'provide', 'including',
                          'increase')
    documents <- tm::tm_map(documents, tm::removeWords, meaningless_word)
    myTdm <- tm::DocumentTermMatrix(documents)
    myTdm2 <- tm::removeSparseTerms(myTdm, sparse = 0.99)
    dtm2list <- apply(myTdm2, 1, function(x) {
      paste(rep(names(x), x), collapse=" ")
    })
    TI_list <- strsplit(dtm2list, "[ ]")
    names(TI_list) <- dfsci$UT
    tiutdf <- na.omit(data.table::data.table(
      'UT'= rep(names(TI_list), sapply(TI_list, length)),
      'TI' =  unlist(TI_list)))
    crutdf.x <- tiutdf
    names(crutdf.x)[1] <- "UT.x"
    setkey(crutdf.x, 'TI')
    crutdf.y  <- tiutdf
    names(crutdf.y)[1] <- "UT.y"
    setkey(crutdf.y, 'TI')
    
    title_length <- as.data.table(dfsci[,c('UT', 'TI')], key = UT)
    title_length$TI <- stringr::str_count(title_length$TI, " ")
    
    couple_df <- crutdf.x[crutdf.y, nomatch=0L, on = 'TI', allow.cartesian = T]
    couple_df <- couple_df[UT.x > UT.y]
    couple_df[,`:=`(count = .N), by = .(UT.x, UT.y)]
    couple_df <- couple_df[!duplicated(couple_df[,c(1,3)]), .(UT.x, UT.y, count)]
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               title_length,
                                               by.x = 'UT.x',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               title_length,
                                               by.x = 'UT.y',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df[, w_ij := count/sqrt(TI.x * TI.y)]
    couple_df <- couple_df[,.(UT.x, UT.y, w_ij)]
    
    # couple_df <- inner_join(tiutdf, tiutdf, by = 'TI') %>%
    #   filter(UT.x > UT.y) %>%
    #   group_by(UT.x, UT.y) %>%
    #   summarise(count = n()) %>%
    #   left_join(dfsci[,c('UT', 'TI')], by = c('UT.x' = 'UT'))%>%
    #   left_join(dfsci[,c('UT', 'TI')], by = c('UT.y' = 'UT'))%>%
    #   mutate(NR.x = str_count(TI.x, " "),
    #          NR.y = str_count(TI.y, " "),
    #          w_ij = count/sqrt(NR.x * NR.y)) %>%
    #   select(UT.x, UT.y, w_ij)
    # couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    # couple_df <- filter(couple_df, w_ij != 0)
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))

    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)

  } else if(coupling_by == 'abc'){
    documents <- tolower(dfsci$AB)
    documents <- tm::Corpus(tm::VectorSource(documents))
    documents <- tm::tm_map(documents, tm::removePunctuation)
    documents <- tm::tm_map(documents, tm::removeNumbers)
    meaningless_word <- c(tm::stopwords("english"), 'use', 'used', 'using', 'uses',
                          'new', 'effect', 'effects', 'affect', 'affects', 'impact',
                          'impacts', 'implication', 'implications', 'potential',
                          'influence', 'influences', 'influenced', 'study', '-',
                          'data', 'can', 'results', 'different', 'similar', 'also',
                          'c', 'may', 'based', 'important', 'within','however',
                          'found', 'analysis', 'changes', 'among', 'large',
                          'number', 'higher', 'well', 'studies', 'total',
                          'increased', 'increases', 'elsevier', 'level', 'many',
                          'rights', 'present', 'will', 'low', 'across', 'showed',
                          'associated', 'approach', 'related', 'provide', 'including',
                          'increase')
    documents <- tm::tm_map(documents, tm::removeWords, meaningless_word)
    myTdm <- tm::DocumentTermMatrix(documents)
    myTdm2 <- tm::removeSparseTerms(myTdm, sparse = 0.99)
    dtm2list <- apply(myTdm2, 1, function(x) {
      paste(rep(names(x), x), collapse=" ")
    })
    AB_list <- strsplit(dtm2list, "[ ]")
    names(AB_list) <- dfsci$UT
    
    abutdf <- na.omit(data.table::data.table(
      'UT'= rep(names(AB_list), sapply(AB_list, length)),
      'AB' =  tolower(unlist(AB_list))))
    abutdf <- abutdf %>% group_by(UT, AB) %>% summarise()
    abutdf <- as.data.table(abutdf, key = 'AB')
    
    crutdf.x <- abutdf
    names(crutdf.x)[1] <- "UT.x"
    setkey(crutdf.x, 'AB')
    crutdf.y  <- abutdf
    names(crutdf.y)[1] <- "UT.y"
    setkey(crutdf.y, 'AB')  
    ab_length <- as.data.table(dfsci[,c('UT', 'AB')], key = UT)
    ab_length$AB <- stringr::str_count(ab_length$AB, " ")
    
    couple_df <- crutdf.x[crutdf.y, nomatch=0L, on = 'AB', allow.cartesian = T]
    couple_df <- couple_df[UT.x > UT.y]
    couple_df[,`:=`(count = .N), by = .(UT.x, UT.y)]
    couple_df <- couple_df[!duplicated(couple_df[,c(1,3)]), .(UT.x, UT.y, count)]
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               ab_length,
                                               by.x = 'UT.x',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               ab_length,
                                               by.x = 'UT.y',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df[, w_ij := count/sqrt(AB.x * AB.y)]
    couple_df <- couple_df[,.(UT.x, UT.y, w_ij)]
    
    # abutdf <- data.frame('UT'= rep(names(AB_list), sapply(AB_list, length)),
    #                      'AB' =  tolower(unlist(AB_list)),
    #                      stringsAsFactors=F)
    # abutdf <- abutdf %>% group_by(UT, AB) %>% summarise()
    # couple_df <- inner_join(abutdf, abutdf, by = 'AB') %>%
    #   filter(UT.x > UT.y) %>%
    #   group_by(UT.x, UT.y) %>%
    #   summarise(count = n()) %>%
    #   left_join(dfsci[,c('UT', 'AB')], by = c('UT.x' = 'UT'))%>%
    #   left_join(dfsci[,c('UT', 'AB')], by = c('UT.y' = 'UT'))%>%
    #   mutate(NR.x = str_count(AB.x, " "),
    #          NR.y = str_count(AB.y, " "),
    #          w_ij = count/sqrt(NR.x * NR.y)) %>%
    #   select(UT.x, UT.y, w_ij) 
    # couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    # couple_df <- filter(couple_df, w_ij != 0)
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))

    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
  } else if(coupling_by == 'joc'){
    
    cr_list <- strsplit(dfsci$CR, split="; ")
    names(cr_list) <- dfsci$UT
    crutdf <- na.omit(data.table::data.table(
      'UT'= rep(names(cr_list), sapply(cr_list, length)),
      'CR' = toupper(unlist(cr_list)),
      key = 'CR'))
    
    splitted_cr <- as.data.table(splitted_cr, key = 'record')
    rm(cr_list)
    tmp <- data.table:::merge.data.table(
      crutdf,
      splitted_cr,
      by.x = 'CR',
      by.y = 'record',
      all.x = T)
    tmp <- tmp[, .(UT, journal)]
    tmp[, jo_freq := .N, , by = .(UT, journal)]
    tmp <- tmp[!duplicated(tmp[,c(1,2)])]
    tmp <- tmp[, v1 :=  .N,by = .(journal)]
    tmp <- tmp[v1 >= 3,]

    crutdf.x <- tmp
    names(crutdf.x)[1] <- "UT.x"
    setkey(crutdf.x, 'journal')
    crutdf.y  <- tmp
    names(crutdf.y)[1] <- "UT.y"
    setkey(crutdf.y, 'journal')
    
    couple_df <- crutdf.x[crutdf.y, nomatch=0L, on = 'journal', allow.cartesian = T][UT.x > UT.y]
    couple_df[,min_jo := min(jo_freq, i.jo_freq)]
    couple_df <- couple_df[, .(UT.x, UT.y, min_jo)]
    couple_df[,`:=`(count = .N), by = .(UT.x, UT.y)]
    couple_df <- couple_df[!duplicated(couple_df[,c(1,2)]), .(UT.x, UT.y, count)]
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               dfsci[,c('UT', 'NR')],
                                               by.x = 'UT.x',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               dfsci[,c('UT', 'NR')],
                                               by.x = 'UT.y',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df[, w_ij := count/sqrt(NR.x * NR.y)]
    couple_df <- couple_df[,.(UT.x, UT.y, w_ij)]
    # cr_list <- strsplit(dfsci$CR, split="; ")
    # names(cr_list) <- dfsci$UT
    # crutdf <- data.frame('UT'= rep(names(cr_list), sapply(cr_list, length)),
    #                      'CR' = toupper(unlist(cr_list)),
    #                      stringsAsFactors=F)
    # rm(cr_list)
    # tmp <- left_join(crutdf, splitted_cr, by = c('CR' = 'record')) %>%
    #   select(UT, journal) %>%
    #   group_by(UT, journal) %>%
    #   summarise(jo_freq = n()) %>%
    #   filter(!is.na(journal))
    # couple_df <- inner_join(tmp, tmp, by = 'journal') %>%
    #   filter(UT.x > UT.y) %>%
    #   mutate(min_jo = min(jo_freq.x, jo_freq.y)) %>%
    #   select(UT.x, UT.y, min_jo) %>%
    #   group_by(UT.x, UT.y) %>%
    #   summarise(count = sum(min_jo)) %>%
    #   left_join(dfsci[,c('UT', 'NR')], by = c('UT.x' = 'UT'))%>%
    #   left_join(dfsci[,c('UT', 'NR')], by = c('UT.y' = 'UT'))%>%
    #   mutate(w_ij = count/sqrt(NR.x * NR.y)) %>%
    #   select(UT.x, UT.y, w_ij)
    # rm(tmp)
    # gc()
    # couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    # couple_df <- filter(couple_df, w_ij != 0)
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
    
    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)

  } else if(coupling_by == 'auc'){
    
    cr_list <- strsplit(dfsci$AU, split="; ")
    names(cr_list) <- dfsci$UT
    crutdf <- na.omit(data.table::data.table(
      'UT'= rep(names(cr_list), sapply(cr_list, length)),
      'CR' = unlist(cr_list)))

    crutdf.x <- crutdf
    names(crutdf.x)[1] <- "UT.x"
    setkey(crutdf.x, 'AU')
    crutdf.y  <- crutdf
    names(crutdf.y)[1] <- "UT.y"
    setkey(crutdf.y, 'AU')
    
    au_length <- as.data.table(dfsci[,c('UT', 'AU')], key = UT)
    au_length$AU <- stringr::str_count(title_length$AU, "; ")
    
    couple_df <- crutdf.x[crutdf.y, nomatch=0L, on = 'AU', allow.cartesian = T]
    couple_df <- couple_df[UT.x > UT.y]
    couple_df[,`:=`(count = .N), by = .(UT.x, UT.y)]
    couple_df <- couple_df[!duplicated(couple_df[,c(1,3)]), .(UT.x, UT.y, count)]
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               au_length,
                                               by.x = 'UT.x',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df <- data.table:::merge.data.table(couple_df, 
                                               au_length,
                                               by.x = 'UT.y',
                                               by.y = 'UT',
                                               all.x=TRUE)
    couple_df[, w_ij := count/sqrt(AU.x * AU.y)]
    couple_df <- couple_df[,.(UT.x, UT.y, w_ij)]
    # cr_list <- strsplit(dfsci$AU, split="; ")
    # names(cr_list) <- dfsci$UT
    # crutdf <- data.frame('UT'= rep(names(cr_list), sapply(cr_list, length)),
    #                      'CR' = unlist(cr_list),
    #                      stringsAsFactors=F)
    # couple_df <- inner_join(crutdf, crutdf, by = 'CR') %>%
    #   filter(UT.x > UT.y) %>%
    #   group_by(UT.x, UT.y) %>%
    #   summarise(count = n()) %>%
    #   left_join(dfsci[,c('UT', 'AU')], by = c('UT.x' = 'UT'))%>%
    #   left_join(dfsci[,c('UT', 'AU')], by = c('UT.y' = 'UT'))%>%
    #   mutate(NR.x = stringr::str_count(AU.x, "; "),
    #          NR.y = stringr::str_count(AU.y, "; "),
    #          w_ij = count/sqrt(NR.x * NR.y)) %>%
    #   select(UT.x, UT.y, w_ij) 
    # couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    # couple_df <- filter(couple_df, w_ij != 0)
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
    
    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
    
    
  } else if(coupling_by == 'woc'){
    couple_df <- wcoupling(dfsci, kw, ti , ab)
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
  } else if(coupling_by == 'bickec'){
    # BIC
    cr_list <- strsplit(dfsci$CR, split="; ")
    names(cr_list) <- dfsci$UT
    crutdf <- data.frame('UT'= rep(names(cr_list), sapply(cr_list, length)),
                         'CR' = unlist(cr_list),
                         stringsAsFactors=F)
    couple_df <- inner_join(crutdf, crutdf, by = 'CR') %>%
      filter(UT.x > UT.y) %>%
      group_by(UT.x, UT.y) %>%
      summarise(count = n()) %>%
      left_join(dfsci[,c('UT', 'NR')], by = c('UT.x' = 'UT'))%>%
      left_join(dfsci[,c('UT', 'NR')], by = c('UT.y' = 'UT'))%>%
      mutate(w_ij = count/sqrt(NR.x * NR.y)) %>%
      select(UT.x, UT.y, w_ij)
    rm(crutdf)
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df_bic <- filter(couple_df, w_ij != 0)
    
    ## KEC
    DE_list <- strsplit(dfsci$DE, "[;][ ]")
    names(DE_list) <- dfsci$UT
    deutdf <- data.frame('UT'= rep(names(DE_list), sapply(DE_list, length)),
                         'KW' =  tolower(unlist(DE_list)),
                         stringsAsFactors=F)
    
    ID_list <- strsplit(dfsci$ID, "[;][ ]")
    names(ID_list) <- dfsci$UT
    idutdf <- data.frame('UT'= rep(names(ID_list), sapply(ID_list, length)),
                         'KW' =  tolower(unlist(ID_list)),
                         stringsAsFactors=F)
    
    kwutdf <- rbind(idutdf, deutdf)
    kw_length <- group_by(kwutdf, UT) %>%
      summarize(NK = n())
    couple_df <- inner_join(kwutdf, kwutdf, by = 'KW') %>%
      filter(UT.x > UT.y) %>%
      group_by(UT.x, UT.y) %>%
      summarise(count = n()) %>%
      left_join(kw_length, by = c('UT.x' = 'UT'))%>%
      left_join(kw_length, by = c('UT.y' = 'UT'))%>%
      mutate(w_ij = count/sqrt(NK.x * NK.y)) %>%
      select(UT.x, UT.y, w_ij)
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df_kec <- filter(couple_df, w_ij != 0)
    
    # Join kec and bic
    couple_df <- full_join(ungroup(couple_df_bic), ungroup(couple_df_kec), by = c('UT.x', 'UT.y'))
    couple_df$w_ij.x[is.na(couple_df$w_ij.x)] <- 0
    couple_df$w_ij.y[is.na(couple_df$w_ij.y)] <- 0
    couple_df <- mutate(couple_df,
                        w_ij = 2 * w_ij.x + w_ij.y) %>%
      select(UT.x, UT.y, w_ij)
    # Keep going normally
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    rm(coup2)
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
    rm(k_i)
    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
    
  }else if(coupling_by == 'bickecticjoc'){
    # BIC
    cr_list <- strsplit(dfsci$CR, split="; ")
    names(cr_list) <- dfsci$UT
    crutdf <- data.frame('UT'= rep(names(cr_list), sapply(cr_list, length)),
                         'CR' = unlist(cr_list),
                         stringsAsFactors=F)
    couple_df <- inner_join(crutdf, crutdf, by = 'CR') %>%
      filter(UT.x > UT.y) %>%
      group_by(UT.x, UT.y) %>%
      summarise(count = n()) %>%
      left_join(dfsci[,c('UT', 'NR')], by = c('UT.x' = 'UT'))%>%
      left_join(dfsci[,c('UT', 'NR')], by = c('UT.y' = 'UT'))%>%
      mutate(w_ij = count/sqrt(NR.x * NR.y)) %>%
      select(UT.x, UT.y, w_ij)
    rm(crutdf)
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df_bic <- filter(couple_df, w_ij != 0)
    
    ## KEC
    DE_list <- strsplit(dfsci$DE, "[;][ ]")
    names(DE_list) <- dfsci$UT
    deutdf <- data.frame('UT'= rep(names(DE_list), sapply(DE_list, length)),
                         'KW' =  tolower(unlist(DE_list)),
                         stringsAsFactors=F)
    
    ID_list <- strsplit(dfsci$ID, "[;][ ]")
    names(ID_list) <- dfsci$UT
    idutdf <- data.frame('UT'= rep(names(ID_list), sapply(ID_list, length)),
                         'KW' =  tolower(unlist(ID_list)),
                         stringsAsFactors=F)
    
    kwutdf <- rbind(idutdf, deutdf)
    kw_length <- group_by(kwutdf, UT) %>%
      summarize(NK = n())
    couple_df <- inner_join(kwutdf, kwutdf, by = 'KW') %>%
      filter(UT.x > UT.y) %>%
      group_by(UT.x, UT.y) %>%
      summarise(count = n()) %>%
      left_join(kw_length, by = c('UT.x' = 'UT'))%>%
      left_join(kw_length, by = c('UT.y' = 'UT'))%>%
      mutate(w_ij = count/sqrt(NK.x * NK.y)) %>%
      select(UT.x, UT.y, w_ij)
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df_kec <- filter(couple_df, w_ij != 0)
    
    # TIC
    documents <- tolower(dfsci$TI)
    documents <- tm::Corpus(tm::VectorSource(documents))
    documents <- tm::tm_map(documents, tm::removePunctuation)
    documents <- tm::tm_map(documents, tm::removeNumbers)
    meaningless_word <- c(tm::stopwords("english"), 'use', 'used', 'using', 'uses',
                          'new', 'effect', 'effects', 'affect', 'affects', 'impact',
                          'impacts', 'implication', 'implications', 'potential',
                          'influence', 'influences', 'influenced', 'study', '-',
                          'data', 'can', 'results', 'different', 'similar', 'also',
                          'c', 'may', 'based', 'important', 'within','however',
                          'found', 'analysis', 'changes', 'among', 'large',
                          'number', 'higher', 'well', 'studies', 'total',
                          'increased', 'increases', 'elsevier', 'level', 'many',
                          'rights', 'present', 'will', 'low', 'across', 'showed',
                          'associated', 'approach', 'related', 'provide', 'including',
                          'increase')
    documents <- tm::tm_map(documents, tm::removeWords, meaningless_word)
    myTdm <- tm::DocumentTermMatrix(documents)
    myTdm2 <- tm::removeSparseTerms(myTdm, sparse = 0.99)
    dtm2list <- apply(myTdm2, 1, function(x) {
      paste(rep(names(x), x), collapse=" ")
    })
    TI_list <- strsplit(dtm2list, "[ ]")
    names(TI_list) <- dfsci$UT
    tiutdf <- data.frame('UT'= rep(names(TI_list), sapply(TI_list, length)),
                         'TI' =  unlist(TI_list),
                         stringsAsFactors=F)
    couple_df <- inner_join(tiutdf, tiutdf, by = 'TI') %>%
      filter(UT.x > UT.y) %>%
      group_by(UT.x, UT.y) %>%
      summarise(count = n()) %>%
      left_join(dfsci[,c('UT', 'TI')], by = c('UT.x' = 'UT'))%>%
      left_join(dfsci[,c('UT', 'TI')], by = c('UT.y' = 'UT'))%>%
      mutate(NR.x = str_count(TI.x, " "),
             NR.y = str_count(TI.y, " "),
             w_ij = count/sqrt(NR.x * NR.y)) %>%
      select(UT.x, UT.y, w_ij)
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df_tic <- filter(couple_df, w_ij != 0)
    
    # JOC
    cr_list <- strsplit(dfsci$CR, split="; ")
    names(cr_list) <- dfsci$UT
    crutdf <- data.frame('UT'= rep(names(cr_list), sapply(cr_list, length)),
                         'CR' = toupper(unlist(cr_list)),
                         stringsAsFactors=F)
    rm(cr_list)
    tmp <- left_join(crutdf, splitted_cr, by = c('CR' = 'record')) %>%
      select(UT, journal) %>%
      group_by(UT, journal) %>%
      summarise(jo_freq = n()) %>%
      filter(!is.na(journal))
    couple_df <- inner_join(tmp, tmp, by = 'journal') %>%
      filter(UT.x > UT.y) %>%
      mutate(min_jo = min(jo_freq.x, jo_freq.y)) %>%
      select(UT.x, UT.y, min_jo) %>%
      group_by(UT.x, UT.y) %>%
      summarise(count = sum(min_jo)) %>%
      left_join(dfsci[,c('UT', 'NR')], by = c('UT.x' = 'UT'))%>%
      left_join(dfsci[,c('UT', 'NR')], by = c('UT.y' = 'UT'))%>%
      mutate(w_ij = count/sqrt(NR.x * NR.y)) %>%
      select(UT.x, UT.y, w_ij)
    rm(tmp)
    gc()
    couple_df$w_ij[couple_df$w_ij == Inf] <- 0
    couple_df_joc <- filter(couple_df, w_ij != 0)
    
    # Join kec, tic and bic
    couple_df <- full_join(ungroup(couple_df_bic), ungroup(couple_df_kec), by = c('UT.x', 'UT.y'), suffix = c(".bic", ".kec")) %>%
      full_join(ungroup(couple_df_tic), by = c('UT.x', 'UT.y'), suffix = c('', '.tic')) %>%
      full_join(ungroup(couple_df_joc), by = c('UT.x', 'UT.y'), suffix = c('', '.joc'))
    couple_df$w_ij.bic[is.na(couple_df$w_ij.bic)] <- 0
    couple_df$w_ij.kec[is.na(couple_df$w_ij.kec)] <- 0
    couple_df$w_ij[is.na(couple_df$w_ij)] <- 0
    couple_df$w_ij.joc[is.na(couple_df$w_ij.joc)] <- 0
    couple_df <- mutate(couple_df,
                        w_ij = 2 * w_ij.bic + w_ij.kec + w_ij + 0.5 * w_ij.joc) %>%
      select(UT.x, UT.y, w_ij)
    # Keep going normally
    m <- sum(couple_df$w_ij)
    coup2 <- data.frame(wos_id = c(as.vector(couple_df$UT.x), as.vector(couple_df$UT.y)),
                        w_ij = rep(couple_df$w_ij,2))
    k_i <- coup2 %>%
      group_by(wos_id) %>%
      summarise(k_i = sum(w_ij))
    rm(coup2)
    nam <- as.character(k_i$wos_id)
    k_i <- k_i$k_i
    names(k_i) <- nam
    couple_df <- couple_df %>%
      mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
    rm(k_i)
    names(couple_df) <- c("rec1",
                          "rec2",
                          "bc",
                          "weight"
    )
    couple_df <- ungroup(couple_df)
    tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))]
    if(length(tmp) >= 1){
      missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(couple_df$rec2, couple_df$rec1))))],
                               'rec2' = dfsci$UT[1],
                               'bc' = 0,
                               'weight' = 0,
                               stringsAsFactors = F)
      couple_df <- rbind(couple_df, missing_df)
    }
    graph <- igraph::graph_from_data_frame(d=couple_df, directed= F)
    
  }
  return(graph)
}
