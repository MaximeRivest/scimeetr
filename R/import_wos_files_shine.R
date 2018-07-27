#' Import and Merge Web of Science Files into a Data Frame
#' 
#' This function imports and if there is more than one file in the user selected
#' directory automatically merges Web of Science files. This is all put into a 
#' data frame. Moreover, all duplicated records are automatically removed.
#' 
#' @usage import_wos_files_shine(files_directory)
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
#' @seealso \code{\link{scimeetr}} and \code{\link{import_scopus_files}}.
#' @keywords manip
#' @import dplyr stringr
import_wos_files_shine <-
  function (folder_content) 
  {
    header_name <- "PT\tAU\tBA\tBE\tGP\tAF\tBF\tCA\tTI\tSO\tSE\tBS\tLA\tDT\tCT\tCY\tCL\tSP\tHO\tDE\tID\tAB\tC1\tRP\tEM\tRI\tOI\tFU\tFX\tCR\tNR\tTC\tZ9\tU1\tU2\tPU\tPI\tPA\tSN\tEI\tBN\tJ9\tJI\tPD\tPY\tVL\tIS\tPN\tSU\tSI\tMA\tBP\tEP\tAR\tDI\tD2\tEA\tEY\tPG\tWC\tSC\tGA\tUT\tPM\tOA\tHC\tHP\tDA"
    dfsci_temp <- NULL
    #folder_content <- list.files(files_directory)
    files_quantity <- length(folder_content)
    for (files in 1:files_quantity) {
      full_file_path <- folder_content[files]
      if (files == 1) {
        fileName <- full_file_path
        v_char <- suppressWarnings(readLines(full_file_path, encoding = "UTF-8"))
        v_char <- iconv(v_char, from = "UTF-8", to = "ASCII", 
                        sub = "")
        # if(v_char[1] != header_name){
        #   v_char = c(header_name, v_char)
        # }
        v_char <- stringr::str_replace(v_char, "^[null]+", "")
        tab_count <- stringr::str_count(v_char[], '\t')
        good_lines <- c(1, which(tab_count == max(tab_count)))
        
        dfsci <-read.table(text = v_char[good_lines] ,header = T, quote = "",
                           fileEncoding = 'ASCII',
                           row.names = NULL,
                           comment.char = "",
                           stringsAsFactors = F,
                           sep = "\t")
      }
      else {
        fileName <- full_file_path
        v_char <- suppressWarnings(readLines(full_file_path, encoding = "UTF-8"))
        v_char <- iconv(v_char, from = "UTF-8", to = "ASCII", 
                        sub = "")
        # if(v_char[1] != header_name){
        #   v_char = c(header_name, v_char)
        # }
        v_char <- stringr::str_replace(v_char, "^[null]+", "")
        tab_count <- stringr::str_count(v_char[], '\t')
        good_lines <- c(1, which(tab_count == max(tab_count)))
        dfsci_temp <-read.table(text = v_char[good_lines] ,header = T, quote = "",
                                fileEncoding = 'ASCII',
                                row.names = NULL,
                                comment.char = "",
                                stringsAsFactors = F,
                                sep = "\t")
        dfsci <- rbind(dfsci, dfsci_temp)
      }
    }
    column_names <- names(dfsci)[-1]
    dfsci <- dfsci[, 1:(ncol(dfsci) - 1)]
    names(dfsci) <- column_names
    if(sum(is.na(dfsci$CR)) == nrow(dfsci)) {
      warning("The field CR (cited reference) contains no references. \nAlmost everything is scimeetr will break without that field. \nWhen you download records from WOS or Scopus, please carefully follow that steps at: \nhttps://github.com/MaximeRivest/scimeetr#how-to-get-bibliometric-data",call. = F)
    } 
    dfsci$RECID <- make_recid(dfsci)
    lsci <- list("com1" = list("dfsci" = dfsci[!duplicated(dfsci), ]))
    class(lsci) <- c('scimeetr', class(lsci))
    lsci <- scimeetr:::add_table_freq(lsci)
    return(lsci)
  }
