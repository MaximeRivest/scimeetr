#' Wrapper of igraph community algorithms
#' 
#' This function imports and if there is more than one file in the user selected
#' directory automatically merges Web of Science files. This is all put into a 
#' data frame. Moreover, all duplicated records are automatically removed.
#' 
#' @usage import_wos_files(files_directory)
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
#' 
#' @keywords manip
#' @export
#' @import dplyr
clusterize <- function(graph = graph, community_algorithm = 'louvain'){
  
  if(community_algorithm == 'louvain'){
    community <- igraph::cluster_louvain(graph)
  } else if(community_algorithm == 'fast greedy'){
    community <- igraph::cluster_fast_greedy(graph)
  }
  return(community)
}

