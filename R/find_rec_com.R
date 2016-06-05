find_rec_com <-
  function (cocitation_df, WOS_dataframe) 
  {
    bc_data_frame <- data.frame(rec1 = cocitation_df$source, 
                                rec2 = cocitation_df$target, BC_weight = cocitation_df$BC_weight)
    non_zero_bcweight <- subset(bc_data_frame, BC_weight != 0)
    non_zero_bcweight$rec1 <- as.character(non_zero_bcweight$rec1)
    non_zero_bcweight$rec2 <- as.character(non_zero_bcweight$rec2)
    source_target_df1 <- data.frame(source = non_zero_bcweight$rec1, 
                                    target = non_zero_bcweight$rec2)
    graph <- igraph::graph_from_edgelist(as.matrix(source_target_df1), 
                                         directed = F)
    community <- igraph::cluster_louvain(graph, weights = non_zero_bcweight[, 
                                                                            3])
    quantity_of_community <- max(igraph::membership(community))
    communities_holder <- list()
    for (communityID in 1:quantity_of_community) {
      communities_holder[[communityID]] <- names(igraph::membership(community)[igraph::membership(community) == 
                                                                                 communityID])
    }
    quantity_of_community <- length(communities_holder)
    reorder_df <- data.frame("pos" = 1:quantity_of_community, "siz" = 
                               sapply(communities_holder, function(x){length(x)}))
    reorder_df <- reorder_df[order(reorder_df$siz, decreasing = T),]
    WOS_dataframe$record_community <- rep(NA, nrow(WOS_dataframe))
    counter <- 1
    for(communityID in reorder_df$pos) {
      length_of_current_community <- length(communities_holder[[communityID]])
      for(current_node in 1:length_of_current_community) {
        holder <- which(
          WOS_dataframe$UT %in% communities_holder[[communityID]][current_node])
        WOS_dataframe$record_community[holder] <- counter
      }
      counter <- counter + 1
    }
    return(WOS_dataframe)
  }