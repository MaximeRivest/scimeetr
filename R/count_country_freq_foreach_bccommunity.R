#------------------------------------------------------------------------------
# list of community with df of most said country
#------------------------------------------------------------------------------
count_country_freq_foreach_bccommunity <- function(WOS_dataframe) {
  
  community_quantity <- max(WOS_dataframe$record_community,na.rm =T)
  country_per_X_community        <- as.list(1:(community_quantity+1))
  for (i in 1:community_quantity) {
    sub_WOS_dataframe <- subset(WOS_dataframe, record_community == i)
    sub_WOS_dataframe <- subset(sub_WOS_dataframe, sub_WOS_dataframe$Countries != "")
    sub_WOS_dataframe <- na.omit(sub_WOS_dataframe$Countries)
    country_current_bccom <- toupper(unlist(strsplit(unlist(sub_WOS_dataframe), "[;]")))
    country_current_bccom <- as.factor(country_current_bccom)
    freq_table_country <- plyr::count(country_current_bccom)
    ordering_command               <- with(freq_table_country, order(-freq))
    freq_table_country              <- freq_table_country[ordering_command, ]
    names(freq_table_country) <- c("Country", "Frequency")
    country_per_X_community[[i]] <- freq_table_country
  }
  WOS_dataframe <- subset(WOS_dataframe, WOS_dataframe$Countries != "")
  WOS_dataframe <- na.omit(WOS_dataframe$Countries)
  country_total <- toupper(unlist(strsplit(unlist(WOS_dataframe), "[;]")))
  country_total <- as.factor(country_total)
  freq_table_country <- plyr::count(country_total)
  ordering_command               <- with(freq_table_country, order(-freq))
  freq_table_country              <- freq_table_country[ordering_command, ]
  names(freq_table_country) <- c("Country", "Frequency")
  country_per_X_community[[community_quantity+1]] <- freq_table_country
  return(country_per_X_community)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------