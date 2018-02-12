#' Split cited references into first author, year, journal, etc.
#' 
#' @param scimeetr_data A scimeetr object.
#' @return a dataframe
#' @importFrom dplyr %>%
#' @export
split_cr <- function(scimeetr_data, min_cr_freq = 2)
{
  parent_com <- purrr::map_df(scimeetr_data, 'dfsci')
  dfsci <- unique(parent_com)
  if(stringr::str_detect(dfsci$UT[1], "^WOS")){
    cr_vec<- table(unlist(stringr::str_split(dfsci$CR, '; ')))
    cr_vec <- toupper(names(cr_vec[cr_vec >= min_cr_freq]))
    row_quantity <- length(cr_vec)
    au <- stringr::str_extract(cr_vec, "^[\\w-]*,*\\s{1}[[:upper:]\\.\\s]*")
    au <- stringr::str_replace_all(au, "\\.", '')
    au <- stringr::str_replace_all(au, ",", '')
    yr <- stringr::str_extract(cr_vec, "(?<=\\s)[12]{1}[0-9]{3}(?=,)")
    so <- stringr::str_extract(cr_vec, "(?<=\\s[12]{1}[0-9]{3},\\s{1,3})[\\w\\s]*")
    vl <- stringr::str_extract(cr_vec, "(?<=\\sV)\\d*(?=,)")
    bp <- stringr::str_extract(cr_vec, "(?<=\\sP)\\d*(?=,)")
    simple_doi <- stringr::str_extract(cr_vec, "(?<=DOI\\s).*")
    x <- paste0(au, ', ', yr, ', ', so, ', ', vl, ', ', bp)
    x <- stringr::str_replace_all(x, ' ,', ',')
    doi <- paste('<a href="https://doi.org/',simple_doi,'">',simple_doi,'</a>', sep = "")
    doi_url <- paste('https://doi.org/',simple_doi, sep = "")
    #x <- x[-which(stringr::str_count(x, "NA")>=4)]
    cr_df <- data.frame(record  = cr_vec,
                        author  = au,
                        year    = yr,
                        journal = so,
                        volume  = vl,
                        page    = bp,
                        doi     = doi,
                        doi_url = doi_url,
                        simple_doi = simple_doi,
                        stringsAsFactors = F)

    cr_df$RECID <- suppressWarnings(paste(cr_df$author, cr_df$year, cr_df$journal, cr_df$volume,
                                           cr_df$page, sep = ", "))
    cr_df$ID <- toupper(cr_df$record)
  } else {
    cr_vec<- table(unlist(stringr::str_split(dfsci$CR, '; ')))
    cr_vec <- toupper(names(cr_vec[cr_vec >= min_cr_freq]))
    row_quantity <- length(cr_vec)
    
    list_sep_cr <- strsplit(cr_vec, split = "[,][[:blank:]]")
    author <- purrr::map_chr(list_sep_cr, .null = NA, 1)
    year <- purrr::map_chr(list_sep_cr, .null = NA,2)
    journal <- purrr::map_chr(list_sep_cr, .null = NA,3)
    volume <- purrr::map_chr(list_sep_cr, .null = NA,4)
    page <- purrr::map_chr(list_sep_cr, .null = NA,5)
    cr_df <- data.frame(record  = cr_vec,
                        author  = author,
                        year    = year,
                        journal = journal,
                        volume  = volume,
                        page    = page,
                        doi     = NA,
                        doi_url = NA,
                        simple_doi = NA,
                        stringsAsFactors = F)
    paste3 <- function(...,sep=", ") {
      L <- list(...)
      L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
      ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                 gsub(paste0(sep,sep),sep,
                      do.call(paste,c(L,list(sep=sep)))))
      is.na(ret) <- ret==""
      ret
    }
    cr_df$RECID <- suppressWarnings(paste3(cr_df$author, cr_df$year, cr_df$journal, cr_df$volume,
                                           cr_df$page, sep = ", "))
    cr_df$ID <- toupper(cr_df$record)
  }
  return(cr_df)
}
