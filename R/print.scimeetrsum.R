#' Print scimeetr summary
#' 
#' @param object 
#' @param ... 
#' @method print scimeetrsum
#' @export
print.scimeetrsum <- function(object, ...){
  cat("\n # Summary of Scimeetr #\n")
  cat("-----------------------\n")
  cat("    Number of papers: ", object$nb_papers)
  cat("\n    Number of different reference: ", object$nb_ref)
  cat("\n\n    Average number of reference per paper: ", object$avg_nb_ref)
  cat("\n\n    Quantiles of total citation per paper: \n\n")
  print(object$quant_nb_citation)
  cat("\n    Mean number of citation per paper: ", object$mean_nb_citation)
  cat("\n\n    Average number of citation per paper per year: ", object$avg_nb_citation_yr)
  cat("\n\n\n  Table of the 10 most mentioned keywords \n\n")
  kw4pr <- as.data.frame(object$top10kw)
  colnames(kw4pr) <- c("                  Keyword", "   Frequency")
  print(kw4pr, right = T)
  cat("\n\n\n  Table of the 10 most productive journal \n\n")
  so4pr <- data.frame(x = names(object$top10so),
                      y = as.vector(object$top10so))
  colnames(so4pr) <- c("                  Journal", "   Frequency")
  print(so4pr, right = T)
  cat("\n\n\n  Table of the most descriminant keywords \n\n")
  print(object$ltag, right = T)
}
