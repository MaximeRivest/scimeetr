#' Plot network of sub-communities
#' 
#' @param scimeetr a scimeetr object
#' @param scimeetr_id a community id that contains a graph
#' @return a Networkvis plot
#' @importFrom dplyr %>%
#' @export
plot_subcommunities <- function(scimeetr, scimeetr_id = "com1") {
  sci_id <- which(names(scimeetr) == scimeetr_id)
  gs <- scimeetr[[sci_id]]$graph
  community <- scimeetr[[sci_id]]$coms
  cl <- community
  igraph::V(gs)$name <- cl$membership
  igraph::V(gs)$weight <- 1
  gcon <- igraph::contract.vertices(gs, cl$membership, 
                            vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))
  # Simplify edges
  gcon <- igraph::simplify(gcon, edge.attr.comb = list(weight = "sum", function(x)length(x)))
  gcc <- igraph::induced.subgraph(gcon, igraph::V(gcon)$weight > 30)
  cut.off <- mean(igraph::E(gcc)$weight) 
  gcc <- igraph::delete_edges(gcc, igraph::E(gcc)[igraph::E(gcc)$weight<(0.33*cut.off)])
  igraph::V(gcc)$degree <- unname(igraph::degree(gcc)) 
  nodes <- data.frame('id'= igraph::as_ids(igraph::V(gcc)),
                      label = paste0(scimeetr_id,'_', igraph::as_ids(igraph::V(gcc))))
  
  links <- igraph::as_data_frame(gcc)
  links$width <- links$weight/min(links$weight)
  visnet <- visNetwork::visNetwork(nodes, links)%>%
    visNetwork::visIgraphLayout(layout = "layout_with_fr") %>%
    visNetwork::visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
    visNetwork::visOptions(highlightNearest = list(enabled = T, hover = T))%>%
    visNetwork::visPhysics(solver = "barnesHut")
  return(visnet)
}