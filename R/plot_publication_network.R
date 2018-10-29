#' Plot publication network colored by sub-communities
#' 
#' With \code{plot_publication_network} you can generate a plot of a publication
#' network which will be colored by sub-communities.
#' 
#' @param scimeetr_data A \link{scimeetr} object
#' @param plot_to_file True or False
#' @param filename a character string matchin a path name
#' @param width Argument to jpeg()
#' @param height Argument to jpeg()
#' @param min_com_size A numeric vector of length one.
#' @param ... if plot_to_file is True, parameter of the jpeg() function.
#' @return A plot, generated from the sankeyNetwork function in the package
#'   networkD3
#' @importFrom pryr %<a-%
#' @export
#' 
plot_publication_network <- function(scimeetr_data, 
                                     plot_to_file = F, 
                                     filename,
                                     width,
                                     height,
                                     min_com_size = 100){
  g <- scimeetr_data[[1]]$graph
  cur_com <- scimeetr_data[[1]]$coms
  cur_dfsci <- scimeetr_data[[1]]$dfsci
  g <- igraph::set.vertex.attribute(g,
                                    'TC',
                                    cur_dfsci$UT,
                                    cur_dfsci$TC)
  g <- igraph::set.vertex.attribute(g,
                                    'MB',
                                    cur_dfsci$UT,
                                    cur_dfsci$MB)
  g <- igraph::set.vertex.attribute(g,
                                    'RECID',
                                    cur_dfsci$UT,
                                    cur_dfsci$RECID)
  comsdf <- as.data.frame(igraph::sizes(cur_com))
  comsdf$Community.sizes <- as.numeric(comsdf$Community.sizes)
  tmp <- dplyr::left_join(cur_dfsci, comsdf, by = c('MB' = 'Community.sizes'))
  g <- igraph::set.vertex.attribute(g,
                                    'CS',
                                    tmp$UT,
                                    tmp$Freq)
  g1 <- igraph::induced_subgraph(g, igraph::V(g)[CS >= min_com_size])
  g1 <- igraph::delete.edges(g1,
                             which(igraph::E(g1)$weight <= 0.3))
  g1 <- igraph::delete.vertices(g1,which(igraph::degree(g1)<3))
  shortpath <- igraph::distances(g1,
                                 v = igraph::V(g1),
                                 to = igraph::V(g1),
                                 mode = "all",
                                 weights = igraph::E(g1)$weight,
                                 algorithm = "dijkstra")
  shortpath[is.infinite(shortpath)] <- 0
  l <- igraph::layout_with_mds(g1, dist = shortpath)
  l <- igraph::norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  g1 <- igraph::delete.edges(g1,
                             which(igraph::E(g1)$weight <= sort(igraph::E(g1)$weight, decreasing = T)[1000]))
  coms <- as.integer(as.factor(as.character(igraph::V(g1)$CS)))
  if(length(unique(coms)) >= 3) {
    pal1 <- RColorBrewer::brewer.pal(length(unique(coms)), "Set1")
  } else {
    pal1 <- c("#E41A1C", "#377EB8")
  }
  
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
            rgb(x[1], x[2], x[3], alpha=alpha))
  }
  igraph::E(g1)$color <- add.alpha(rgb(0.9, 0.9, 0.9), 0.2)
  n <- c(0, unique(as.numeric(as.factor(as.character(igraph::V(g1)$CS)))))
  plots <- as.list(n)
  if(plot_to_file == F) {
    par(mfrow = c(ceiling((max(n)+1)/3), 3))
    for(i in n) {
      igraph::V(g1)$color <- purrr::map2_chr(as.list(pal1[coms]), as.list(log(igraph::V(g1)$TC+1)/(max(log(igraph::V(g1)$TC+1)))), add.alpha)
      if(i != 0){
        igraph::V(g1)$color[as.numeric(as.factor(as.character(igraph::V(g1)$CS))) != i] <- add.alpha(rgb(0.95, 0.95, 0.95), 0.1)
      }
      plot(g1,
           vertex.frame.color=igraph::V(g1)$color,
           vertex.frame.size=0.1,
           edge.size=.1,
           vertex.label=NA,
           vertex.size= (log(igraph::V(g1)$TC+1)/log(max(igraph::V(g1)$TC)+1))*10,
           edge.curved=0.3,
           layout = l * 1,
           rescale = F
      )
    }
    plot.new()
    legend(x=0.25, y=0.75, levels(as.factor(as.character(igraph::V(g1)$CS))), pch=21,
           col="grey", pt.bg=pal1, pt.cex=1.5, cex=1.5, bty="n", ncol=1)
  } else {
    jpeg(filename = filename, width, height)
    par(mfrow = c(ceiling((max(n)+1)/3), 3))
    for(i in n) {
      igraph::V(g1)$color <- purrr::map2_chr(as.list(pal1[coms]), as.list(log(igraph::V(g1)$TC+1)/(max(log(igraph::V(g1)$TC+1)))), add.alpha)
      if(i != 0){
        igraph::V(g1)$color[as.numeric(as.factor(as.character(igraph::V(g1)$CS))) != i] <- add.alpha(rgb(0.95, 0.95, 0.95), 0.1)
      }
      plot(g1,
           vertex.frame.color=igraph::V(g1)$color,
           vertex.frame.size=0.1,
           edge.size=.1,
           vertex.label=NA,
           vertex.size= (log(igraph::V(g1)$TC+1)/log(max(igraph::V(g1)$TC)+1))*10,
           edge.curved=0.3,
           layout = l * 1,
           rescale = F
      )
    }
    plot.new()
    legend(x=0.25, y=0.75, levels(as.factor(as.character(igraph::V(g1)$CS))), pch=21,
           col="grey", pt.bg=pal1, pt.cex=1.5, cex=1.5, bty="n", ncol=1) 
    dev.off()
  }
  par(mfrow = c(1,1))
  igraph::V(g1)$color <- purrr::map2_chr(as.list(pal1[coms]), as.list(log(igraph::V(g1)$TC+1)/(max(log(igraph::V(g1)$TC+1)))), add.alpha)
  return(invisible(list(g1, l)))
}