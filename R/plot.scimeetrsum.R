#' Plot scimeetr summary
#' 
#' @param object a summary object
#' @param ... igraph plot options
#' @method plot scimeetrsum
#' @importFrom igraph plot.igraph
#' @export
plot.scimeetrsum <- function(object, node_names = 'tag', node_size = 1, ...){
  if(any(names(object) =='plot')){
    subnet <- object$plot
    igraph::plot.igraph(subnet,                  
                        vertex.label.color="black",
                        vertex.shape = 'rectangle',
                        vertex.color = 'grey',
                        edge.arrow.size=.2,
                        vertex.label=igraph::get.vertex.attribute(subnet, node_names),
                        vertex.label.cex=0.5 * node_size,
                        vertex.size= 120 * node_size,
                        vertex.size2= 50 * node_size,
                        edge.curved=0.2,
                        layout = igraph::layout_with_kk(subnet),
                        margin = 0 * node_size,
                        asp = 1)
    invisible(subnet)
  } else {
    warning("There is no graph. \nThis is probably because you did not run scimap() yet. \nSo, there is no graph associated to com1.", call. = FALSE)
  }
}