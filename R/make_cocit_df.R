make_cocit_df <-
  function (WOS_dataframe) 
  {
    # Index the cited Ref
    for (i in ncol(WOS_dataframe)) {
      if (is.factor(WOS_dataframe[, i])) {
        WOS_dataframe[, i] <- as.character(WOS_dataframe[, i])
      }
    }
    cr_list <- strsplit(WOS_dataframe$CR, split = "; ")
    cr_quant_per_record <- sapply(cr_list, function(x) length(x))
    indexed_cr_vector <- as.integer(as.factor(unlist(cr_list)))
    indexed_o_sep_cr_vec <- indexed_cr_vector
    minus_last_cr_quant_per_record <- cr_quant_per_record[1:length(cr_quant_per_record) - 
                                                            1]
    position_counter <- minus_last_cr_quant_per_record
    position_of_appendage <- rep(0, length(position_counter))
    for (n in 1:length(position_counter)) {
      position_of_appendage[n] <- sum(position_counter[1:n]) + 
        n - 1
      indexed_o_sep_cr_vec <- append(indexed_o_sep_cr_vec, 
                                     values = 0, after = position_of_appendage[n])
    }
    unique_indexed_o_sep_cr_vec <- indexed_o_sep_cr_vec[duplicated(indexed_o_sep_cr_vec) | 
                                                          duplicated(indexed_o_sep_cr_vec, fromLast = T)]
    unique_indexed_1_sep_cr_vec <- as.integer(as.factor(unique_indexed_o_sep_cr_vec))
    separator_position <- which(unique_indexed_1_sep_cr_vec == 
                                  1)
    indexed_cr_list <- as.list(1:nrow(WOS_dataframe))
    end_boundary <- separator_position - 1
    end_boundary <- end_boundary[2:length(separator_position)]
    start_boundary <- separator_position + 1
    start_boundary <- start_boundary[1:length(separator_position) - 
                                       1]
    indexed_cr_list[[nrow(WOS_dataframe)]] <- unique_indexed_1_sep_cr_vec[(separator_position[length(separator_position)] + 
                                                                         1):length(unique_indexed_1_sep_cr_vec)]
    indexed_cr_list[[1]] <- unique_indexed_1_sep_cr_vec[1:separator_position[1] - 
                                                          1]
    for (m in 1:length(start_boundary)) {
      indexed_cr_list[[m + 1]] <- unique_indexed_1_sep_cr_vec[start_boundary[m]:end_boundary[m]]
    }
    indexed_cr_list <- sapply(indexed_cr_list, function(x) {
      na.omit(x)
    })
    #Use the indexed CR to make the cocitation table
    options(bigmemory.typecast.warning = FALSE)
    for (i in ncol(WOS_dataframe)) {
      if (is.factor(WOS_dataframe[, i])) {
        WOS_dataframe[, i] <- as.character(WOS_dataframe[, i])
      }
    }
    max_expected_combinations <- choose(nrow(WOS_dataframe), 2)
    number_commun_references <- big.matrix(max_expected_combinations, 
                                           1, type = "integer", init = 0, shared = FALSE)
    cr_matrix <- big.matrix(nrow(WOS_dataframe), length(unique(unlist(indexed_cr_list))) + 
                              1, type = "integer", init = 0, shared = FALSE)
    counter <- 0
    for (x in 1:length(indexed_cr_list)) {
      counter <- counter + 1
      cr_matrix[counter, indexed_cr_list[[x]]] <- cr_matrix[counter, 
                                                            indexed_cr_list[[x]]] + 1
    }
    counter_combinations <- 2:nrow(cr_matrix)
    position_counter <- c((nrow(WOS_dataframe) - 1):1)
    position_counter_2 <- rep(0, length(position_counter) - 1)
    position_counter_s <- rep(0, length(position_counter) - 1)
    for (p in 1:length(position_counter)) {
      position_counter_2[p] <- sum(position_counter[1:p])
    }
    position_counter_s <- position_counter_2 + 1
    position_counter_s <- append(position_counter_s, 1, 0)
    position_counter_s <- position_counter_s[1:(length(position_counter_s) - 
                                                  1)]
    cat("\r", "                                                      ")
    for (rec in 1:length(position_counter_2)) {
      number_commun_references[position_counter_s[rec]:position_counter_2[rec], 
                               ] <- cr_matrix[counter_combinations[rec]:nrow(cr_matrix), 
                                              ] %*% cr_matrix[rec, ]
      cat("\r", rec, " of ", length(position_counter_2))
    }
    rec_vector <- WOS_dataframe$UT
    source_target_mat <- t(combn(rec_vector, 2))
    nref <- as.matrix(number_commun_references)
    cocitation_df <- data.table::data.table(source = source_target_mat[, 
                                                              1], target = source_target_mat[, 2], number_of_references_source = rep(0, 
                                                                                                                                     max_expected_combinations), number_of_references_target = rep(0, 
                                                                                                                                                                                                   max_expected_combinations), number_of_references_in_commun = nref)
    WOS_dataframe <- as.data.table(WOS_dataframe)
    setkey(WOS_dataframe, UT)
    setkey(cocitation_df, target)
    for (rec in rec_vector) {
      cocitation_df[.(rec), `:=`(number_of_references_target, 
                                    WOS_dataframe[rec, NR])]
    }
    setkey(cocitation_df, source)
    for (rec in rec_vector) {
      cocitation_df[.(rec), `:=`(number_of_references_source, 
                                    WOS_dataframe[rec, NR])]
    }
    cocitation_df <- cocitation_df[number_of_references_in_commun != 
                                           0]
    WOS_dataframe <- as.data.frame(WOS_dataframe)
    cocitation_df <- as.data.frame(cocitation_df)
    w_ij <- cocitation_df$number_of_references_in_commun/sqrt(cocitation_df$number_of_references_source * cocitation_df$number_of_references_target)
    w_ij[w_ij==Inf] <- 0
    cocitation_df$BC_weight <- w_ij
    return(cocitation_df)
  }