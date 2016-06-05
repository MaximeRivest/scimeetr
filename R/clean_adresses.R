clean_adresses <- 
  function(WOS_dataframe)
    {
    #------------------------------------------------------------------------------
    # Add a column a ; seperated universities per Record
    #------------------------------------------------------------------------------
    make_univ_column <- function(WOS_dataframe) {
      
      C1vec <- WOS_dataframe$C1
      univ_final_vec <- rep(NA, nrow(WOS_dataframe))
      for (i in 1:nrow(WOS_dataframe)) {
        if(length(stringr::str_extract_all(C1vec[i], pattern = "\\]\\s[[:alnum:]\\s]{1,}")[[1]]) != 0){
          univ1 <- stringr::str_locate_all(C1vec[i], pattern = "\\]\\s[[:alnum:]\\s]{1,}")
          univ1[[1]][,1] <- univ1[[1]][,1] + 2
          univec <- sapply(row(univ1[[1]])[,1], function(x){substr(C1vec[i], univ1[[1]][x,1], univ1[[1]][x,2])})
          univec <- unique(univec)
          univ_final_vec[i] <- paste(univec, collapse = ";")
        } else if (length(stringr::str_extract_all(C1vec[i], pattern = "^[[:alnum:]\\s]{1,}")[[1]]) != 0) {
          univ1 <- stringr::str_extract_all(C1vec[i], pattern = "^[[:alnum:]\\s]{1,}")
          univ2 <- stringr::str_locate_all(C1vec[i], pattern = ";\\s[[:alnum:]\\s]{1,}")
          if(nrow(univ2[[1]]) != 0) { 
            univ2[[1]][,1] <- univ2[[1]][,1] + 2
            univ2 <- sapply(row(univ2[[1]])[,1], function(x){substr(C1vec[i], univ2[[1]][x,1], univ2[[1]][x,2])})
          } else { univ2 <- NULL}
          univec<- unique(c(univ1, univ2))
          univ_final_vec[i] <- paste(univec, collapse = ";")
        }
      }
      WOS_dataframe$Universities <- univ_final_vec
      WOS_dataframe$Universities <- as.character(WOS_dataframe$Universities)
      return(WOS_dataframe)
    }
    #------------------------------------------------------------------------------
    # Add a column a ; seperated countries per Record
    #------------------------------------------------------------------------------
    make_country_column <- function(WOS_dataframe) {
      
      C1vec <- WOS_dataframe$C1
      country_final_vec <- rep(NA, nrow(WOS_dataframe))
      for (i in 1:nrow(WOS_dataframe)) {
        if(length(stringr::str_extract_all(C1vec[i], pattern = "[:alnum:]{1,};\\s\\[")[[1]]) != 0){
          univ1 <- stringr::str_locate_all(C1vec[i], pattern = "[:alnum:]{1,};\\s\\[")
          univ1[[1]][,2] <- univ1[[1]][,2] - 3
          univ2 <- stringr::str_extract(C1vec[i], pattern = "\\w*$")
          univec <- sapply(row(univ1[[1]])[,1], function(x){substr(C1vec[i], univ1[[1]][x,1], univ1[[1]][x,2])})
          univec <- unique(c(univec, univ2))
          if(!all(is.na(stringr::str_extract(univec, pattern = "[:digit:]")))) {
            country_final_vec[i] <- "USA"
          } else {
            country_final_vec[i] <- paste(univec, collapse = ";")
          }
        } else if (length(stringr::str_extract_all(C1vec[i], pattern = "\\w*$")[[1]]) != 0) {
          univ1 <- stringr::str_extract(C1vec[i], pattern = "\\w*$")
          univ2 <- stringr::str_locate_all(C1vec[i], pattern = "[:alpha:]{1,};")
          if(nrow(univ2[[1]]) != 0 & length(stringr::str_extract(C1vec[i], pattern = "\\]")) == 0 ) { 
            univ2[[1]][,2] <- univ2[[1]][,2] - 1
            univ2 <- sapply(row(univ2[[1]])[,1], function(x){substr(C1vec[i], univ2[[1]][x,1], univ2[[1]][x,2])})
          } else { univ2 <- NULL}
          univec<- unique(c(univ1, univ2))
          if(!any(is.na(stringr::str_extract(univec, pattern = "[:digit:]")))) {
            country_final_vec[i] <- "USA"
          } else {
            country_final_vec[i] <- paste(univec, collapse = ";")
          }
        }
      }
      
      WOS_dataframe$Countries <- country_final_vec
      WOS_dataframe$Countries <- as.character(WOS_dataframe$Countries)
      return(WOS_dataframe)
    }
    WOS_dataframe <- make_univ_column(WOS_dataframe)
    WOS_dataframe <- make_country_column(WOS_dataframe)
    return(WOS_dataframe)
  }
