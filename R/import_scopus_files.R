#' Import and Merge Scopus Files into a scimeetr list
#' 
#' This function imports and if there is more than one file in the user selected
#' directory automatically merges Web of Science files. This is all put into a 
#' data frame. Moreover, all duplicated records are automatically removed.
#' 
#' @usage import_scopus_files(files_directory)
#' @param files_directory a character vector giving the \bold{folder} path in 
#'   which all the Web of Science files to be imported into a data frame can be 
#'   found. This folder should contain \bold{only} the files to be imported.
#' @details No details for now.
#' @return data frame of 62 columns and a number of row equal to the number of 
#'   unique records.
#' @author Maxime Rivest
#' @keywords manip
#' @seealso \code{\link{scimeetr}} and \code{\link{import_wos_files}}.
#' @export
#' @import dplyr stringr
import_scopus_files <- function(files_directory) {
  dfsci_temp <- NULL
  folder_content <- list.files(files_directory)
  files_quantity <- length(folder_content)
  for (files in 1:files_quantity) {
    full_file_path <- paste(files_directory, folder_content[files], 
                            sep = "")
    if (files == 1) {
      fileName <- full_file_path
      v_char <- suppressWarnings(readLines(full_file_path, encoding = "UTF-8"))
      v_char <- iconv(v_char, from = "UTF-8", to = "ASCII", 
                      sub = "")
      #tab_count <- stringr::str_count(v_char[], ',')
      #good_lines <- c(1, which(tab_count == max(tab_count)))
      dfsci <-read.table(text = v_char, header = T, quote = '"',
                         fileEncoding = 'ASCII',
                         row.names = NULL,
                         comment.char = "",
                         stringsAsFactors = F,
                         sep = ",")
    }
    else {
      fileName <- full_file_path
      v_char <- suppressWarnings(readLines(full_file_path, encoding = "UTF-8"))
      v_char <- iconv(v_char, from = "UTF-8", to = "ASCII", 
                      sub = "")
      #tab_count <- stringr::str_count(v_char[], ',')
      #good_lines <- c(1, which(tab_count == max(tab_count)))
      dfsci_temp <-read.table(text = v_char,header = T, quote = '"',
                              fileEncoding = 'ASCII',
                              row.names = NULL,
                              comment.char = "",
                              stringsAsFactors = F,
                              sep = ",")
      dfsci <- rbind(dfsci, dfsci_temp)
    }
  }
  dfsci <- unique(dfsci)
  names(dfsci)[names(dfsci) == "References"] <- "CR"
  names(dfsci)[names(dfsci) == "EID"] <- "UT"
  names(dfsci)[names(dfsci) == "Affiliations"] <- "C1"
  names(dfsci)[names(dfsci) == "Author.Keywords"] <- "DE"
  names(dfsci)[names(dfsci) == "Index.Keywords"] <- "ID"
  names(dfsci)[names(dfsci) == "Authors"] <- "AU"
  names(dfsci)[names(dfsci) == "Abbreviated.Source.Title"] <- "JI"
  names(dfsci)[names(dfsci) == "Source.tilte"] <- "SO"
  names(dfsci)[names(dfsci) == "Volume"] <- "VL"
  names(dfsci)[names(dfsci) == "Year"] <- "PY"
  names(dfsci)[names(dfsci) == "Page.start"] <- "BP"
  names(dfsci)[names(dfsci) == "Title"] <- "TI"
  names(dfsci)[names(dfsci) == "Abstract"] <- "AB"
  names(dfsci)[names(dfsci) == "Cited.by"] <- "TC"
  names(dfsci)[names(dfsci) == "Source.title"] <- "SO"
  dfsci$J9 <- stringr::str_replace_all(toupper(dfsci$JI), "\\.", '')
  names(dfsci)[names(dfsci) == "DOI"] <- "DI"
  #to change , for ; in authors
  dfsci$AU <- gsub(",", ";", dfsci$AU)
  #To calculate number of references
  cr_list <- strsplit(toupper(dfsci$CR), split="; ")
  dfsci$NR <- sapply(cr_list, function(x)length(x))
  dfsci$RECID <- make_recid(dfsci)
  
  #Clean scopus CR
  CR <- purrr::map(cr_list, function(x){
    au <- stringr::str_extract(x, "^\\w*,*\\s{1}[[:upper:]\\.]*")
    au <- stringr::str_replace_all(au, "\\.", '')
    au <- stringr::str_replace_all(au, ",", '')
    yr <- stringr::str_extract(x, "(?<=\\()[12]{1}[0-9]{3}(?=\\))|[12]{1}[0-9]{3}(?=\\))|(?<=\\()[12]{1}[0-9]{3}")
    so <- stringr::str_extract(x, "(?<=\\([12]{1}[0-9]{3}\\)\\s{1,3})[\\w\\.\\s]*")
    so <- stringr::str_replace_all(so, "\\.", '')
    vl <- stringr::str_extract(x, "(?<=\\s)\\d{1,6}(?=,\\sPP\\.\\s)|(?<=\\s)\\d{1,6}(?=\\s\\({1}[0-9]{1,5}\\){1},\\sPP\\.\\s)")
    bp <- stringr::str_extract(x, "(?<=PP\\.\\s)[0-9]+")
    x <- paste0(au, ', ', yr, ', ', so, ', ', vl, ', ', bp)
    x <- stringr::str_replace_all(x, ' ,', ',')
    x <- x[-which(stringr::str_count(x, "NA")>=4)]
  })
  dfsci$CR <- purrr::map_chr(CR, paste, collapse = '; ')
  dfsci$CR[dfsci$CR == ""] <- NA
  
  id_list <- strsplit(dfsci$CR, "[;][ ]")
  iddf <- data.frame('ID' = unlist(id_list),
                     'UT' = rep(dfsci$UT, sapply(id_list, length)),
                     stringsAsFactors=F)
  cr_vec<- stringr::str_split(iddf$ID, ', ')
  iddf <- iddf[purrr::map_int(cr_vec, length) == 5,]
  cr_vec <- purrr::keep(cr_vec, purrr::map_int(cr_vec, length) == 5)
  tmp <- as.data.frame(do.call(rbind, cr_vec), stringsAsFactors = F)
  names(tmp) <- c('AU', 'PY', 'SO', 'VL', 'BP')
  iddf <- cbind(iddf, tmp)
  iddf <- iddf[!is.na(iddf$SO),]
  iddf$SO <- stringr::str_replace_all(iddf$SO, '\\sTHE\\s|\\sOR\\s|\\sAND\\s|\\sOF\\s|\\sA\\s|\\sAN\\s', ' ')
  
  tabso <- table(iddf$SO)
  tmp <- data.frame(old_so = names(tabso[as.logical(tabso>2)]),
                    new_so = "",
                    stringsAsFactors = F)
  tmp <- tmp[!stringr::str_detect(tmp$old_so, "^[:alpha:]*\\s*$"),]

  vregex <- stringr::str_replace_all(tmp$old_so[order(nchar(tmp$old_so), decreasing = T)], "\\s", "[:alpha:]*?\\\\s")
  vregex <- paste0("^", vregex, "[[:alpha:]\\s]*?")
  
  for(i in 1:length(vregex)){
    pos_same_so <- stringr::str_which(tmp$old_so, vregex[i])
    if(length(pos_same_so)>=2){
      vso <- tmp$old_so[pos_same_so]
      nso <- nchar(tmp$old_so[pos_same_so])
      names(nso) <- vso
      rep_so <- names(sort(nso)[1])
      
      tmp$new_so[pos_same_so] <- names(sort(nso)[1])
    }
  }
  tmp$new_so[tmp$new_so == ""] <- tmp$old_so[tmp$new_so == ""]

  iddftmp <- dplyr::left_join(iddf, tmp, by = c("SO" = "old_so"))
  iddftmp$SO[!is.na(iddftmp$new_so)] <- iddftmp$new_so[!is.na(iddftmp$new_so)]
  iddftmp$CR_new <- paste(iddftmp$AU,iddftmp$PY,iddftmp$SO,iddftmp$VL, iddftmp$BP, sep = ", ")
  tmp <- group_by(iddftmp, UT) %>%
    summarise(CR = paste(CR_new, collapse = '; '))
  dfsci <- select(dfsci, -CR)
  dfsci <- dplyr::left_join(dfsci, tmp, by = "UT")
  lsci <- list("com1" = list("dfsci" = dfsci[!duplicated(dfsci), ]))
  class(lsci) <- c('scimeetr', class(lsci))
  lsci <- add_table_freq(lsci)
  return(lsci)
}
