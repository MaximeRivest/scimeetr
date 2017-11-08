toBibliometrix <- function(WOS_table){
  WOS_table$DB <- 'ISI'
  WOSbib <- WOS_table[,c("AU", "TI", "SO", "JI", "DT", "DE", "ID", "AB", "C1", "CR", "TC", "PY", "SC", "UT", "RP", "DB")]
  for(i in 1:ncol(WOSbib)){
    if(is.factor(WOSbib[,i])){
      WOSbib[,i] <- as.character(WOSbib[,i])
    }
  }
  for(i in 1:ncol(WOSbib)){
    if(is.character(WOSbib[,i])){
      WOSbib[,i] <- toupper(WOSbib[,i])
    }
  }
  return(WOSbib)
}