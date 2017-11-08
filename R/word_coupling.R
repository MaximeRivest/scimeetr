#' Word coupling
#' 
#' This function imports and if there is more than one file in the user selected
#' directory automatically merges Web of Science files. This is all put into a 
#' data frame. Moreover, all duplicated records are automatically removed.
#' 
#' @usage import_wos_files(files_directory)
#' @param files_directory a character vector giving the \bold{folder} path in 
#'   which all the Web of Science files to be imported into a data frame can be 
#'   found. This folder should contain \bold{only} the files to be imported.
#' @details No details for now.
#' @return data frame of 62 columns and a number of row equal to the number of 
#'   unique records.
#' @author Maxime Rivest
#' @examples 
#' \dontrun{Since this example shows how to load WOS from your system we need to run the following line to find the path to the exemple file} 
#' fpath <- system.file("extdata", package="scimeetr") 
#' fpath <- paste(fpath, "/wos_folder/", sep = "") 
#' \dontrun{Then we can run the actual example} 
#' wos_df <- import_wos_files(files_directory = fpath)
#' 
#' @keywords manip
#' @export
#' @import dplyr
wcoupling <- function(kw = 5, ti = 3, ab = 2){
  # Abstract
  documents <- tolower(biblio_df$AB)
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
  names(AB_list) <- biblio_df$UT
  abutdf <- data.frame('UT'= rep(names(AB_list), sapply(AB_list, length)), 
                       'AB' =  tolower(unlist(AB_list)),
                       stringsAsFactors=F)
  abutdf <- abutdf %>% group_by(UT, AB) %>% summarise()
  couple_df <- inner_join(abutdf, abutdf, by = 'AB') %>%
    filter(UT.x > UT.y) %>%
    group_by(UT.x, UT.y) %>%
    summarise(count = n()) %>%
    left_join(biblio_df[,c('UT', 'AB')], by = c('UT.x' = 'UT'))%>%
    left_join(biblio_df[,c('UT', 'AB')], by = c('UT.y' = 'UT'))%>%
    mutate(NR.x = str_count(AB.x, " "),
           NR.y = str_count(AB.y, " "),
           w_ij = count/sqrt(NR.x * NR.y)) %>%
    select(UT.x, UT.y, w_ij)
  couple_df$w_ij[couple_df$w_ij == Inf] <- 0
  ab_couple_df <- filter(couple_df, w_ij != 0)
  names(ab_couple_df) <- c("rec1", 
                           "rec2",
                           "weight"
  )
  # Title
  documents <- tolower(biblio_df$TI)
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
  names(TI_list) <- biblio_df$UT
  tiutdf <- data.frame('UT'= rep(names(TI_list), sapply(TI_list, length)), 
                       'TI' =  unlist(TI_list),
                       stringsAsFactors=F)
  couple_df <- inner_join(tiutdf, tiutdf, by = 'TI') %>%
    filter(UT.x > UT.y) %>%
    group_by(UT.x, UT.y) %>%
    summarise(count = n()) %>%
    left_join(biblio_df[,c('UT', 'TI')], by = c('UT.x' = 'UT'))%>%
    left_join(biblio_df[,c('UT', 'TI')], by = c('UT.y' = 'UT'))%>%
    mutate(NR.x = str_count(TI.x, " "),
           NR.y = str_count(TI.y, " "),
           w_ij = count/sqrt(NR.x * NR.y)) %>%
    select(UT.x, UT.y, w_ij)
  couple_df$w_ij[couple_df$w_ij == Inf] <- 0
  ti_couple_df <- filter(couple_df, w_ij != 0)
  names(ti_couple_df) <- c("rec1", 
                           "rec2",
                           "weight"
  )
  # Keywords
  DE_list <- strsplit(biblio_df$DE, "[;][ ]")
  names(DE_list) <- biblio_df$UT
  deutdf <- data.frame('UT'= rep(names(DE_list), sapply(DE_list, length)), 
                       'KW' =  tolower(unlist(DE_list)),
                       stringsAsFactors=F)
  
  ID_list <- strsplit(biblio_df$ID, "[;][ ]")
  names(ID_list) <- biblio_df$UT
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
  kw_couple_df <- filter(couple_df, w_ij != 0)
  names(kw_couple_df) <- c("rec1", 
                           "rec2",
                           "weight"
  )
  # combine KW, TI, AB
  full_couple_df <- full_join(ungroup(kw_couple_df), ungroup(ti_couple_df),
                              by = c('rec1', 'rec2')) %>%
    full_join(ungroup(ab_couple_df),
              by = c('rec1', 'rec2'))
  aw <- kw + ti + ab
  full_couple_df$w.x[is.na(full_couple_df$w.x)] <- 0
  full_couple_df$w[is.na(full_couple_df$w)] <- 0
  full_couple_df$w.y[is.na(full_couple_df$w.y)] <- 0
  couple_df <- full_couple_df %>%
    mutate(w = kw/aw * w.x + ti/aw * w.y + ab/aw * w) %>%
    select(rec1, rec2, w)
  names(couple_df)[3] <- 'weight'
  return(couple_df)
}