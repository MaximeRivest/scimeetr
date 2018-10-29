#' Print scimeetr
#' 
#' @param object 
#' @param ... 
#' @method print scimeetr
#' @export
print.scimeetr <- function(object, ...){
  cat("\n# A scimeetr object #\n")
  cat("---------------------\n")
  cat("Number of papers: ", nrow(object[[1]]$dfsci))
  cat("\nNumber of communities: ", length(object))
  cat("\nNames of communities: ", names(object))
  cat("\n\n")
  cat("Table of the 5 most mentionned words \n\n")
  df <- data.frame(key_words = object[[1]]$kw$ID[1:5],
                   "title_words" = object[[1]]$ti$ID[1:5],
                   "abstract_words" = object[[1]]$ab$ID[1:5], 
                   stringsAsFactors = F)
  
  print(df, right = T)
}
