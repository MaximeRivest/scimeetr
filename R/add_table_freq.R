#' Make frequencies table
#'
#' @param scimeetr_data An object of class scimeetr
#' @return An object of class scimeetr
#' @import dplyr purrr
add_table_freq <- function(scimeetr_data){
  if(!any(class(scimeetr_data) == "scimeetr")) {
    cat("This is not a scimeetr object !")
  } else {
    names_lsci <- names(scimeetr_data)
    lsci_temp <- list()
    for(i in 1:length(scimeetr_data)){
      i_lsci <- scimeetr_data[[i]]
      if(!any(names(i_lsci) == "kw")){
        DE_list <- strsplit(i_lsci$dfsci$DE, "[;][ ]")
        ID_list <- strsplit(i_lsci$dfsci$ID, "[;][ ]")
        kwdf <- data.frame('ID' = toupper(c(unlist(ID_list),
                                            unlist(DE_list))),
                           stringsAsFactors=F)
        kwdf <- kwdf %>%
          group_by(ID) %>%
          summarise("Frequency.x" = n()) %>%
          ungroup() %>%
          mutate("Pourcentage" = Frequency.x / sum(Frequency.x)) %>%
          arrange(desc(Frequency.x))
        if(!purrr::is_empty(i_lsci$parent_com)) {
          kwdf$comsize <- nrow(i_lsci$dfsci)
          kwdf$parentsize <- nrow(scimeetr_data[[i_lsci$parent_com]]$dfsci)
          kwdf <- kwdf %>%
            left_join(select(scimeetr_data[[i_lsci$parent_com]]$kw,ID, Frequency = Frequency.x), by = "ID") %>%
            mutate("Relative_frequency" = (Frequency.x / (comsize/parentsize * Frequency))) %>%
            arrange(desc(Frequency.x), desc(Relative_frequency))
          #Descriminant words
          i_lsci$tag <-arrange(kwdf[1:25,], desc(Relative_frequency * Frequency.x))$ID[1:6]
        } else {
          i_lsci$tag <- kwdf$ID[1:6]
        }
        i_lsci$kw <- kwdf
      }
      if(!any(names(i_lsci) == "de")){
        DE_list <- strsplit(i_lsci$dfsci$DE, "[;][ ]")
        kwdf <- data.frame('ID' = toupper(unlist(DE_list)),
                           stringsAsFactors=F)
        kwdf <- kwdf %>%
          group_by(ID) %>%
          summarise("Frequency.x" = n()) %>%
          ungroup() %>%
          mutate("Pourcentage" = Frequency.x / sum(Frequency.x)) %>%
          arrange(desc(Frequency.x))
        if(!purrr::is_empty(i_lsci$parent_com)) {
          kwdf$comsize <- nrow(i_lsci$dfsci)
          kwdf$parentsize <- nrow(scimeetr_data[[i_lsci$parent_com]]$dfsci)
          kwdf <- kwdf %>%
            left_join(select(scimeetr_data[[i_lsci$parent_com]]$kw,ID, Frequency = Frequency.x), by = "ID") %>%
            mutate("Relative_frequency" = (Frequency.x / (comsize/parentsize * Frequency))) %>%
            arrange(desc(Frequency.x), desc(Relative_frequency))
        }
        i_lsci$de <- kwdf
      }
      if (!any(names(i_lsci) == "ti")) {
        documents <- tolower(i_lsci$dfsci$TI)
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

        tidf <- data.frame('ID' =  toupper(unlist(TI_list)),
                           stringsAsFactors=F)
        tidf <- tidf %>%
          group_by(ID) %>%
          summarise("Frequency.x" = n()) %>%
          ungroup() %>%
          mutate("Pourcentage" = Frequency.x / sum(Frequency.x)) %>%
          arrange(desc(Frequency.x))
        if(!purrr::is_empty(i_lsci$parent_com)) {
          tidf$comsize <- nrow(i_lsci$dfsci)
          tidf$parentsize <- nrow(scimeetr_data[[i_lsci$parent_com]]$dfsci)
          tidf <- tidf %>%
            left_join(select(scimeetr_data[[i_lsci$parent_com]]$ti, ID, Frequency = Frequency.x), by = "ID") %>%
            mutate("Relative_frequency" = (Frequency.x / (comsize/parentsize * Frequency))) %>%
            arrange(desc(Frequency.x), desc(Relative_frequency))
        }
        i_lsci$ti <- tidf
      }
      if (!any(names(i_lsci) == "ab")) {
        documents <- tolower(i_lsci$dfsci$AB)
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

        abdf <- data.frame('ID' =  toupper(unlist(AB_list)),
                           stringsAsFactors=F)
        abdf <- abdf %>%
          group_by(ID) %>%
          summarise("Frequency.x" = n()) %>%
          ungroup() %>%
          mutate("Pourcentage" = Frequency.x / sum(Frequency.x)) %>%
          arrange(desc(Frequency.x))
        if(!purrr::is_empty(i_lsci$parent_com)) {
          abdf$comsize <- nrow(i_lsci$dfsci)
          abdf$parentsize <- nrow(scimeetr_data[[i_lsci$parent_com]]$dfsci)
          abdf <- abdf %>%
            left_join(select(scimeetr_data[[i_lsci$parent_com]]$ab, ID, Frequency = Frequency.x), by = "ID") %>%
            mutate("Relative_frequency" = (Frequency.x / (comsize/parentsize * Frequency))) %>%
            arrange(desc(Frequency.x), desc(Relative_frequency))
        }
        i_lsci$ab <- abdf
      }
      # if (!any(names(i_lsci) == "wo")) {
      #   ti <- i_lsci$ti
      #   ti$w <- 3
      #   ab <- i_lsci$ab
      #   ab$w <- 2
      #   kw <- i_lsci$kw
      #   kw$w <- 5
      #   kw <- kw[, names(ab)]
      #   wodf <- rbind(kw, ti, ab)
      #   i_lsci$wo <- wodf
      #   wodf1 <- wodf %>%
      #     mutate(Frequency = Frequency * w) %>%
      #     group_by(ID) %>%
      #     summarise(Frequency = sum(Frequency)) %>%
      #     ungroup() %>%
      #     mutate(Pourcentage = Frequency / sum(Frequency))
      #   if(!purrr::is_empty(i_lsci$parent_com)) {
      #     wodf <- wodf %>%
      #       left_join(lsci[[i_lsci$parent_com]]$wo, by = "ID") %>%
      #       mutate("Relative_frequency" = Pourcentage.x / Pourcentage.y) %>%
      #       arrange(desc(Frequency.x), desc(Relative_frequency))
      #   }
      #   i_lsci$wo <- wodf
      # }
      if (!any(names(i_lsci) == "cr") & (sum(is.na(i_lsci$dfsci$CR)) != nrow(i_lsci$dfsci))) {
        id_list <- strsplit(i_lsci$dfsci$CR, "[;][ ]")
        iddf <- data.frame('ID' = toupper(unlist(id_list)),
                           stringsAsFactors=F)
        iddf <- iddf %>%
          group_by(ID) %>%
          summarise("Frequency.x" = n()) %>%
          ungroup() %>%
          mutate("Pourcentage" = Frequency.x / sum(Frequency.x)) %>%
          arrange(desc(Frequency.x))
        if(!purrr::is_empty(i_lsci$parent_com)) {
          iddf$comsize <- nrow(i_lsci$dfsci)
          iddf$parentsize <- nrow(scimeetr_data[[i_lsci$parent_com]]$dfsci)
          iddf <- iddf %>%
            left_join(select(scimeetr_data[[i_lsci$parent_com]]$cr,ID, Frequency = Frequency.x), by = "ID") %>%
            mutate("Relative_frequency" = (Frequency.x / (comsize/parentsize * Frequency))) %>%
            arrange(desc(Frequency.x), desc(Relative_frequency))
        }
        i_lsci$cr <- iddf
      }
      if (!any(names(i_lsci) == "au")) {
        id_list <- strsplit(i_lsci$dfsci$AU, "[;][ ]")
        iddf <- data.frame('ID' = toupper(unlist(id_list)),
                           stringsAsFactors=F)
        iddf <- iddf %>%
          group_by(ID) %>%
          summarise("Frequency.x" = n()) %>%
          ungroup() %>%
          mutate("Pourcentage" = Frequency.x / sum(Frequency.x)) %>%
          arrange(desc(Frequency.x))
        if(!purrr::is_empty(i_lsci$parent_com)) {
          iddf$comsize <- nrow(i_lsci$dfsci)
          iddf$parentsize <- nrow(scimeetr_data[[i_lsci$parent_com]]$dfsci)
          iddf <- iddf %>%
            left_join(select(scimeetr_data[[i_lsci$parent_com]]$au,ID, Frequency = Frequency.x), by = "ID") %>%
            mutate("Relative_frequency" = (Frequency.x / (comsize/parentsize * Frequency))) %>%
            arrange(desc(Frequency.x), desc(Relative_frequency))
        }
        i_lsci$au <- iddf
      }
      if (!any(names(i_lsci) == "co")) {
      }
      if (!any(names(i_lsci) == "yr")) {
      }
      i_lsci <- list(i_lsci)
      names(i_lsci) <- names_lsci[i]
      lsci_temp <- c(lsci_temp, i_lsci)
    }
    class(lsci_temp) <- c('scimeetr', class(lsci_temp))
    kw <- characterize_kw(lsci_temp)
    for(i in 1:length(lsci_temp)){
      lsci_temp[[i]]$tag <- kw[[i]]$keyword[1:6]
    }
    return(lsci_temp)
  }
}
