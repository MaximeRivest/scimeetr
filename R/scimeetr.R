#' Scientometrics Meet R.
#' 
#' \pkg{scimeetr} provides several tools to analyse a corpus of scientific 
#' articles. When used in a systematic way, I argue that scimeetr can provide 
#' and systematic and effecient introduction to a new research field. With 
#' scimeetr a researcher can do several coupling and clustering of scientific 
#' papers. Scimeetr can also be used to calculate several metrics about that 
#' scientific corpus (e.g. most prolific journals, authors, country, 
#' universities, most cited paper, paper citing most other papers, papers with 
#' citation number farthest from expectations). Although these metrics can be 
#' used and calculated independently scimeetr's functions were design to 
#' facilitate the completion of a complete and cohesive analysis of a corpus of 
#' scholarly articles which should be usefull for a novice to a research field.
#' 
#' A classic workflow to use \pkg{scimeetr} will be as follow. (1) Import 
#' bibliometric data that were acquired from \url{https://www.scopus.com/} or 
#' \url{http://apps.webofknowledge.com/} using the function 
#' \code{\link{import_scopus_files}} or the function
#' \code{\link{import_wos_files}} respectively. (2) find research community with
#' \code{\link{scimap}}. (3) Characterize each research communities with
#' \code{\link{summary.scimeetr}}, \code{\link{characterize_kw}},
#' \code{\link{characterize_ti}}, \code{\link{characterize_ab}},
#' \code{\link{characterize_jo}}, \code{\link{characterize_un}}, and
#' \code{\link{characterize_au}}. And, (4) generate reading list with
#' \code{\link{scilist}} or \code{\link{scilist_all}}
#' 
#' For more details, refer to the vignette.
"_PACKAGE"
#> [1] "_PACKAGE"