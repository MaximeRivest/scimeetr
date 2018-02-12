#' Wrapper of igraph community algorithms
#' 
#' A function that helps you pick some community clustering algorithms provided
#' by the package \link{igraph}.
#' 
#' @param graph A igraph object.
#' @param community_algorithm A character vector of length one.
#' @return An object of class 'community'.
#' @import dplyr
clusterize <- function(graph = graph, community_algorithm = 'louvain'){
  if(community_algorithm == 'louvain'){
    community <- igraph::cluster_louvain(graph)
  } else if(community_algorithm == 'fast greedy'){
    community <- igraph::cluster_fast_greedy(graph)
  }
  return(community)
}

