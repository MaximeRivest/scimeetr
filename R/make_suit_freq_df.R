make_suit_freq_df <- 
  function (WOS_dataframe) 
  {
    for (i in ncol(WOS_dataframe)) {
      if (is.factor(WOS_dataframe[, i])) {
        WOS_dataframe[, i] <- as.character(WOS_dataframe[, 
                                                         i])
      }
    }
    country_freq_df <- count_country_freq_foreach_bccommunity(WOS_dataframe)
    country_freq_df <- calculate_relative_frequency(country_freq_df)
    keyword_freq_df <- count_keyword_freq_foreach_bccommunity(WOS_dataframe)
    keyword_freq_df <- calculate_relative_frequency(keyword_freq_df)
    university_freq_df <- count_university_freq_foreach_bccommunity(WOS_dataframe)
    university_freq_df <- calculate_relative_frequency(university_freq_df)
    suit_freq_df <- count_suit_of_freq_foreach_bccommunity(WOS_dataframe)
    for (i in 1:length(suit_freq_df)) {
      suit_freq_df[[i]] <- calculate_relative_frequency(suit_freq_df[[i]])
    }
    suit_freq_df$KW <- keyword_freq_df
    suit_freq_df$UN <- university_freq_df
    suit_freq_df$CO <- country_freq_df
    return(suit_freq_df)
  }