#' Make a recid that math the RECID from the function split_cr()
#' 
#' @param dfsci a dataframe with a CR column
#' @return a vector of same length as input df
#' @import stringr
#' @export
make_recid <- function(WOS_table) {

  paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
  }
  cr_formation <- WOS_table[, c("AU", "PY", "J9", "VL", "BP", "DI")]
  #to get only the fist author
  for(auteur in 1:nrow(cr_formation)){
    end_position <- str_locate(cr_formation$AU[auteur], "[;]")[1]-1
    if(!is.na(end_position)){
      cr_formation$AU[auteur] <- substr(cr_formation$AU[auteur], 1,end_position)
    }
  }
  #to change , for ' ' in authors
  cr_formation$AU <- gsub(",", "", cr_formation$AU)
  cr_formation$AU <- gsub("\\.", "", cr_formation$AU)
  #cr_formation$VL <- paste("V", cr_formation$VL, sep = "")
  #cr_formation$BP <- paste("P", cr_formation$BP, sep = "")
  #for(doi in 1:nrow(cr_formation)){
  #  if(length(cr_formation$DI[doi]) == 0){
  #    cr_formation$DI[doi] <- NA
  #  } else{
  #    cr_formation$DI[doi] <- paste("DOI ", cr_formation$DI[doi], sep = "")
  #  }
  #}
  #cr_formation$BP <- gsub("PNA", NA, cr_formation$BP)
  recid <- toupper(paste3(cr_formation$AU, cr_formation$PY, cr_formation$J9, cr_formation$VL, cr_formation$BP, sep = ", "))
  #whole_cr <- paste(WOS_table$CR_format, collapse = "; ")
  #WOS_table[nrow(WOS_table)+1,] <- NA
  #WOS_table$CR[nrow(WOS_table)] <- whole_cr
  #WOS_table$UT[nrow(WOS_table)] <- "WOS:main"
  #WOS_table$NR[nrow(WOS_table)] <- nrow(cr_formation)
  #WOS_table$CR_format[nrow(WOS_table)] <- "main"
  return(recid)
}
