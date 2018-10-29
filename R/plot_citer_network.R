#' Plot citer network
#' 
#' \link{plot_cite_network} let you plot a network of papers for which only the 
#' papers citing the papers in the reading list are colored.
#' 
#' @param scimeetr_data A list of scimeet object
#' @param reading_list A dataframe from scilist[[i]] or scilist_all[[i]] with a 
#'   publication column
#' @param plot_to_file True or False
#' @param filename a character string matchin a path name
#' @param width Argument to jpeg()
#' @param height Argument to jpeg()
#' @param ... if plot_to_file is True, parameter of the jpeg() function.
#' @return A plot, generated from the sankeyNetwork function in the package 
#'   networkD3
#' @importFrom pryr %<a-%
#' @export
#' @seealso See \link{plot_publication_network} to generate a full publication
#'   plot colored by sub-communities of research.
plot_citer_network <- function(scimeetr_data, reading_list, plot_to_file = F,
                               filename, width, height) {
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
            rgb(x[1], x[2], x[3], alpha=alpha))
  }
  grl <- plot_publication_network(scimeetr_data)
  g1 <- grl[[1]]
  l <- grl[[2]] 
  old_color <- igraph::V(g1)$color
  if(plot_to_file == F) {
    par(mfrow = c(2,5))
    for(i in 1:10){
      igraph::V(g1)$color <- old_color
      tmp_df <- find_citer(scimeetr_data, reading_list$publication[i])
      igraph::V(g1)$color[!(names(igraph::V(g1)) %in% tmp_df$UT)] <- add.alpha(rgb(0.95, 0.95, 0.95), 0.03)
      plot(g1,
           edge.color = rgb(0.9, 0.9, 0.9, alpha = 0.001),
           vertex.frame.color=igraph::V(g1)$color,
           vertex.frame.size=0.1,
           edge.size=.1,
           vertex.label=NA,
           vertex.size= (log(igraph::V(g1)$TC+1)/log(max(igraph::V(g1)$TC)+1))*10,
           edge.curved=0.3,
           layout = l * 1,
           xlim = c(-1,1),
           ylim = c(-1,1),
           rescale = F,
           margin = c(0.01,0.01,0.01,0.01),
           main = as.character(reading_list$publication[i])
      )
    }
  }
  else {
    jpeg(filename, width, height)
    par(mfrow = c(2,5))
    for(i in 1:10){
      igraph::V(g1)$color <- old_color
      tmp_df <- find_citer(scimeetr_data, reading_list$publication[i])
      igraph::V(g1)$color[!(names(igraph::V(g1)) %in% tmp_df$UT)] <- add.alpha(rgb(0.95, 0.95, 0.95), 0.03)
      plot(g1,
           edge.color = rgb(0.9, 0.9, 0.9, alpha = 0.001),
           vertex.frame.color=igraph::V(g1)$color,
           vertex.frame.size=0.1,
           edge.size=.1,
           vertex.label=NA,
           vertex.size= (log(igraph::V(g1)$TC+1)/log(max(igraph::V(g1)$TC)+1))*10,
           edge.curved=0.3,
           layout = l * 1,
           xlim = c(-1,1),
           ylim = c(-1,1),
           rescale = F,
           margin = c(0.01,0.01,0.01,0.01),
           main = as.character(reading_list$publication[i])
      )
    }
    dev.off()
  }
  par(mfrow = c(1,1))
  return(invisible(NULL))
}
