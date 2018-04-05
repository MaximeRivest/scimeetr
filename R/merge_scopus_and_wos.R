#' Merge data from scopus and Web of Science together
#'
#' With \code{merge_scopus_and_wos} you can merge bibliometric data from Scopus
#' and the Web of Science. Be carefull if you are using this prior to
#' bibliographic coupling. Scopus and WOS format the cited reference field
#' differently.
#'
#' @param scopus A \link{scimeetr} object from \link{import_scopus_files}()
#' @param wos A \link{scimeetr} object from \link{import_wos_files}()
#' @return A list of class scimeetr.
#' @details I thank Dr. Shinichi Nakagawa and Rose O'Dea for requiring this
#'   feature.
#' @export
#' 
merge_scopus_and_wos <- function(scopus, wos){
  col2keep <- names(scopus$com1$dfsci)[names(scopus$com1$dfsci) %in% names(wos$com1$dfsci)]
  sub_scopus <- scopus$com1$dfsci[col2keep]
  sub_scopus$fa <- tolower(stringr::str_extract(sub_scopus$AU, '^[A-Za-z]+'))
  sub_scopus$fal <- tolower(sapply(stringr::str_extract_all(sub_scopus$AU, '^[\\w.]+|;\\s[\\w.]+'),paste0, collapse = ''))
  sub_scopus$TI <- toupper(sub_scopus$TI)
  sub_scopus <- sub_scopus[!duplicated(
    with(sub_scopus,
         paste0(PY, VL, J9, fa, TI))),]
  
  sub_wos <- wos$com1$dfsci[col2keep]
  sub_wos$fa <- tolower(stringr::str_extract(sub_wos$AU, '^[A-Za-z]+'))
  sub_wos$fal <- tolower(sapply(stringr::str_extract_all(sub_wos$AU, '^[\\w.]+|;\\s[\\w.]+'),paste0, collapse = ''))
  sub_wos$TI <- toupper(sub_wos$TI)
  sub_wos <- sub_wos[!duplicated(
    with(sub_wos,
         paste0(PY, VL, J9, fa, TI))),]

  tmp1 <- dplyr::inner_join(sub_wos, sub_scopus,by = c("PY", "VL", "J9", "fal", "BP"), suffix = c("", ".scop"))

  tmp2 <- dplyr::inner_join(sub_wos, sub_scopus,by = c("PY", "fa", "TI"), suffix = c("", ".scop"))
  
  tmp3 <- dplyr::inner_join(sub_wos[sub_wos$DI != "",], 
                            sub_scopus[sub_scopus$DI != "",],
                            by = c("DI"), suffix = c("", ".scop"))

  ut_match <- unique(c(tmp1$UT, tmp2$UT, tmp3$UT))
  scop_drop <- unique(c(tmp1$UT.scop, tmp2$UT.scop, tmp3$UT.scop))
  
  sub_scopus <- dplyr::filter(sub_scopus, !(sub_scopus$UT %in% scop_drop))
  
  both_db <- rbind(sub_scopus, sub_wos)
  both_db <- both_db[!duplicated(both_db$DI),]
  both_db <- dplyr::select(both_db, -c(fa, fal))
  for(j in 1:ncol(both_db)){
    if(is.character(both_db[,j]) & (names(both_db)[j] != "DI")){
      both_db[,j] <- toupper(both_db[,j])
    }
  }
  lsci <- as.scimeetr(both_db)
  return(lsci)
}
