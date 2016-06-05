#------------------------------------------------------------------------------
# list of community with df of most Cited references
#------------------------------------------------------------------------------
count_suit_of_freq_foreach_bccommunity <- function(WOS_dataframe) {
  
  community_quantity <- max(WOS_dataframe$record_community,na.rm =T)
  X_per_X_community        <- list("AU" = as.list(1:(community_quantity+1)),
                                   "CR" = as.list(1:(community_quantity+1)),
                                   "SO" = as.list(1:(community_quantity+1)),
                                   "JI" = as.list(1:(community_quantity+1)),
                                   "PY" = as.list(1:(community_quantity+1)),
                                   "WC" = as.list(1:(community_quantity+1)),
                                   "SC" = as.list(1:(community_quantity+1))) 
  for(j in names(X_per_X_community)){
    for (i in 1:community_quantity) {   
      sub_WOS_dataframe <- subset(WOS_dataframe, record_community == i)
      column_picker <- names(sub_WOS_dataframe) == j
      keyword_current_bccom <- tolower(unlist(strsplit(unlist(as.vector(as.character(sub_WOS_dataframe[,which(column_picker)]))), "[;][ ]")))
      keyword_current_bccom <- as.factor(keyword_current_bccom)
      freq_table_keyword <- plyr::count(keyword_current_bccom)
      ordering_command               <- with(freq_table_keyword, order(-freq))
      freq_table             <- freq_table_keyword[ordering_command, ]
      list_picker <- names(X_per_X_community) == j
      if (j == "AU") {
        names(freq_table) <- c("Most Prolific Author", "Frequency")
      } else if (j == "CR"){
        names(freq_table) <- c("Most Cited Article", "Frequency")
      } else if (j == "SO"){
        names(freq_table) <- c("Most Published in", "Frequency")
      } else if (j == "JI"){
        names(freq_table) <- c("Most Published in", "Frequency")
      } else if (j == "PY"){
        names(freq_table) <- c("Year Published", "Frequency")
      } else if (j == "WC"){
        names(freq_table) <- c("Most said Category", "Frequency")
      } else if (j == "SC"){
        names(freq_table) <- c("Reasearch Area", "Frequency")
      }
      X_per_X_community[[which(list_picker)]][[i]] <- freq_table
    }
    sub_WOS_dataframe <- WOS_dataframe
    column_picker <- names(sub_WOS_dataframe) == j
    keyword_current_bccom <- tolower(unlist(strsplit(unlist(as.vector(as.character(sub_WOS_dataframe[,which(column_picker)]))), "[;][ ]")))
    keyword_current_bccom <- as.factor(keyword_current_bccom)
    freq_table_keyword <- plyr::count(keyword_current_bccom)
    ordering_command               <- with(freq_table_keyword, order(-freq))
    freq_table             <- freq_table_keyword[ordering_command, ]
    list_picker <- names(X_per_X_community) == j
    if (j == "AU") {
      names(freq_table) <- c("Most Prolific Author", "Frequency")
    } else if (j == "CR"){
      names(freq_table) <- c("Most Cited Article", "Frequency")
    } else if (j == "SO"){
      names(freq_table) <- c("Most Published in", "Frequency")
    } else if (j == "JI"){
      names(freq_table) <- c("Most Published in", "Frequency")
    } else if (j == "PY"){
      names(freq_table) <- c("Year Published", "Frequency")
    } else if (j == "WC"){
      names(freq_table) <- c("Most said Category", "Frequency")
    } else if (j == "SC"){
      names(freq_table) <- c("Reasearch Area", "Frequency")
    }
    X_per_X_community[[which(list_picker)]][[(community_quantity+1)]] <- freq_table
  }
  return(X_per_X_community)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
