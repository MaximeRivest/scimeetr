#' Word coupling
#' 
#' This function make an adjencency table that is a combination of kw, ti ,ab words.
#' 
#' @param dfsci a dataframe that resides within a community within a scimeetr object
#' @details No details for now.
#' @return data frame of 62 columns and a number of row equal to the number of 
#'   unique records.
#' @author Maxime Rivest
#' @examples 
#' wcoupling(scimeetr_list$com1$dfsci, kw = 2, ti = 1 ab = 1)
#' @import dplyr
wcoupling <- function(dfsci, kw = 5, ti = 3, ab = 2){
  # Abstract
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
  abutdf <- data.frame('UT'= rep(names(AB_list), sapply(AB_list, length)), 
                       'AB' =  tolower(unlist(AB_list)),
                       stringsAsFactors=F)
  abutdf <- abutdf %>% group_by(UT, AB) %>% summarise()
  ab_couple_df <- inner_join(abutdf, abutdf, by = 'AB') %>%
    filter(UT.x > UT.y) %>%
    group_by(UT.x, UT.y) %>%
    summarise(count = n()) %>%
    left_join(dfsci[,c('UT', 'AB')], by = c('UT.x' = 'UT'))%>%
    left_join(dfsci[,c('UT', 'AB')], by = c('UT.y' = 'UT'))%>%
    mutate(NR.x = stringr::str_count(AB.x, " "),
           NR.y = stringr::str_count(AB.y, " "),
           w_ij = count/sqrt(NR.x * NR.y)) %>%
    select(UT.x, UT.y, w_ij)
  ab_couple_df$w_ij[ab_couple_df$w_ij == Inf] <- 0
  ab_couple_df <- filter(ab_couple_df, w_ij != 0)
  m <- sum(ab_couple_df$w_ij)
  coup2 <- data.frame(wos_id = c(as.vector(ab_couple_df$UT.x), as.vector(ab_couple_df$UT.y)),
                      w_ij = rep(ab_couple_df$w_ij,2))
  k_i <- coup2 %>%
    group_by(wos_id) %>%
    summarise(k_i = sum(w_ij))
  nam <- as.character(k_i$wos_id)
  k_i <- k_i$k_i
  names(k_i) <- nam
  ab_couple_df <- ab_couple_df %>%
    mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
  
  names(ab_couple_df) <- c("rec1",
                           "rec2",
                           "bc",
                           "weight"
  )
  ab_couple_df <- ungroup(ab_couple_df)
  tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(ab_couple_df$rec2, ab_couple_df$rec1))))]
  if(length(tmp) >= 1){
    missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(ab_couple_df$rec2, ab_couple_df$rec1))))],
                             'rec2' = dfsci$UT[1],
                             'bc' = 0,
                             'weight' = 0,
                             stringsAsFactors = F)
    ab_couple_df <- rbind(ab_couple_df, missing_df)
  }
  # Title
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
  ti_couple_df <- inner_join(tiutdf, tiutdf, by = 'TI') %>%
    filter(UT.x > UT.y) %>%
    group_by(UT.x, UT.y) %>%
    summarise(count = n()) %>%
    left_join(dfsci[,c('UT', 'TI')], by = c('UT.x' = 'UT'))%>%
    left_join(dfsci[,c('UT', 'TI')], by = c('UT.y' = 'UT'))%>%
    mutate(NR.x = stringr::str_count(TI.x, " "),
           NR.y = stringr::str_count(TI.y, " "),
           w_ij = count/sqrt(NR.x * NR.y)) %>%
    select(UT.x, UT.y, w_ij)
  ti_couple_df$w_ij[ti_couple_df$w_ij == Inf] <- 0
  ti_couple_df <- filter(ti_couple_df, w_ij != 0)
  m <- sum(ti_couple_df$w_ij)
  coup2 <- data.frame(wos_id = c(as.vector(ti_couple_df$UT.x), as.vector(ti_couple_df$UT.y)),
                      w_ij = rep(ti_couple_df$w_ij,2))
  k_i <- coup2 %>%
    group_by(wos_id) %>%
    summarise(k_i = sum(w_ij))
  nam <- as.character(k_i$wos_id)
  k_i <- k_i$k_i
  names(k_i) <- nam
  ti_couple_df <- ti_couple_df %>%
    mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
  
  names(ti_couple_df) <- c("rec1",
                           "rec2",
                           "bc",
                           "weight"
  )
  ti_couple_df <- ungroup(ti_couple_df)
  tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(ti_couple_df$rec2, ti_couple_df$rec1))))]
  if(length(tmp) >= 1){
    missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(ti_couple_df$rec2, ti_couple_df$rec1))))],
                             'rec2' = dfsci$UT[1],
                             'bc' = 0,
                             'weight' = 0,
                             stringsAsFactors = F)
    ti_couple_df <- rbind(ti_couple_df, missing_df)
  }
  # Keywords
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
  kw_couple_df <- inner_join(kwutdf, kwutdf, by = 'KW') %>%
    filter(UT.x > UT.y) %>%
    group_by(UT.x, UT.y) %>%
    summarise(count = n()) %>%
    left_join(kw_length, by = c('UT.x' = 'UT'))%>%
    left_join(kw_length, by = c('UT.y' = 'UT'))%>%
    mutate(w_ij = count/sqrt(NK.x * NK.y)) %>%
    select(UT.x, UT.y, w_ij)
  kw_couple_df$w_ij[kw_couple_df$w_ij == Inf] <- 0
  kw_couple_df <- filter(kw_couple_df, w_ij != 0)
  m <- sum(kw_couple_df$w_ij)
  coup2 <- data.frame(wos_id = c(as.vector(kw_couple_df$UT.x), as.vector(kw_couple_df$UT.y)),
                      w_ij = rep(kw_couple_df$w_ij,2))
  k_i <- coup2 %>%
    group_by(wos_id) %>%
    summarise(k_i = sum(w_ij))
  nam <- as.character(k_i$wos_id)
  k_i <- k_i$k_i
  names(k_i) <- nam
  kw_couple_df <- kw_couple_df %>%
    mutate(asso_stre = (2* w_ij * m)/ (k_i[UT.x] * k_i[UT.y]))
  
  names(kw_couple_df) <- c("rec1",
                           "rec2",
                           "bc",
                           "weight"
  )
  kw_couple_df <- ungroup(kw_couple_df)
  tmp <- dfsci$UT[which(!(dfsci$UT %in% unique(c(kw_couple_df$rec2, kw_couple_df$rec1))))]
  if(length(tmp) >= 1){
    missing_df <- data.frame('rec1' = dfsci$UT[which(!(dfsci$UT %in% unique(c(kw_couple_df$rec2, kw_couple_df$rec1))))],
                             'rec2' = dfsci$UT[1],
                             'bc' = 0,
                             'weight' = 0,
                             stringsAsFactors = F)
    kw_couple_df <- rbind(kw_couple_df, missing_df)
  }
  # combine KW, TI, AB
  full_couple_df <- full_join(ungroup(kw_couple_df), ungroup(ti_couple_df),
                              by = c('rec1', 'rec2')) %>%
    full_join(ungroup(ab_couple_df),
              by = c('rec1', 'rec2'))
  aw <- kw + ti + ab
  full_couple_df$weight.x[is.na(full_couple_df$weight.x)] <- 0
  full_couple_df$weight[is.na(full_couple_df$weight)] <- 0
  full_couple_df$weight.y[is.na(full_couple_df$weight.y)] <- 0
  couple_df <- full_couple_df %>%
    mutate(weight = kw/aw * weight.x + ti/aw * weight.y + ab/aw * weight) %>%
    select(rec1, rec2, weight)
  names(couple_df)[3] <- 'weight'
  return(couple_df)
}