#' Plot scimeetr summary
#' 
#' @param object 
#' @param ... 
#' @method plot scimeetrsum
#' @export
#' @importFrom igraph plot.igraph
plot.scimeetrsum <- function(object, node_names = 'tag', ...){
  subnet <- object$plot
  igraph::plot.igraph(subnet,
       vertex.label.color="black",
       edge.arrow.size=.1,
       vertex.shape="none",
       vertex.label=igraph::get.vertex.attribute(subnet,node_names),
       vertex.label.cex=.4,
       vertex.size=25,
       edge.curved=0.2,
       layout = igraph::layout_with_kk(subnet))
}
