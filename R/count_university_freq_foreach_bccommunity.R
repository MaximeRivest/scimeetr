#------------------------------------------------------------------------------
# list of community with df of most said university
#------------------------------------------------------------------------------
count_university_freq_foreach_bccommunity <- function(WOS_dataframe) {
  
  community_quantity <- max(WOS_dataframe$record_community,na.rm =T)
  university_per_X_community        <- as.list(1:community_quantity)
  for (i in 1:community_quantity) {
    sub_WOS_dataframe <- subset(WOS_dataframe, record_community == i)
    sub_WOS_dataframe <- subset(sub_WOS_dataframe, sub_WOS_dataframe$Universities != "")
    sub_WOS_dataframe <- na.omit(sub_WOS_dataframe$Universities)
    university_current_bccom <- toupper(unlist(strsplit(unlist(sub_WOS_dataframe), "[;]")))
    university_current_bccom <- as.factor(university_current_bccom)
    freq_table_university <- plyr::count(university_current_bccom)
    ordering_command               <- with(freq_table_university, order(-freq))
    freq_table_university              <- freq_table_university[ordering_command, ]
    names(freq_table_university) <- c("University", "Frequency")
    university_per_X_community[[i]] <- freq_table_university
  }
  WOS_dataframe <- subset(WOS_dataframe, WOS_dataframe$Universities != "")
  WOS_dataframe <- na.omit(WOS_dataframe$Universities)
  university_total <- toupper(unlist(strsplit(unlist(WOS_dataframe), "[;]")))
  university_total <- as.factor(university_total)
  freq_table_university <- plyr::count(university_total)
  ordering_command               <- with(freq_table_university, order(-freq))
  freq_table_university              <- freq_table_university[ordering_command, ]
  names(freq_table_university) <- c("University", "Frequency")
  university_per_X_community[[community_quantity+1]] <- freq_table_university
  return(university_per_X_community)
}