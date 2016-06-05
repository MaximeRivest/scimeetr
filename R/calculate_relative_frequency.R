#------------------------------------------------------------------------------
# Calculate weight ratio
#------------------------------------------------------------------------------
calculate_relative_frequency <- function(freq_df) {
  dt_tot <- as.data.table(freq_df[[length(freq_df)]])
  key_name <- names(freq_df[[length(freq_df)]])[1]
  for(i in 1:length(freq_df)) {
    dt <- as.data.table(freq_df[[i]])
    setkeyv(dt, key_name)
    setkeyv(dt_tot, key_name)
    new_df <- dt_tot[dt]
    sum_tot <- sum(new_df$Frequency)
    sum_sub <- sum(new_df$i.Frequency)
    new_df$rel_freq <- (new_df$i.Frequency/sum_sub)/(new_df$Frequency/sum_tot)
    setorder(new_df, -i.Frequency, -rel_freq)
    new_df <- as.data.frame(new_df)
    names(new_df)[3:4] <-  c("Frequency", "Relative Frequency")
    freq_df[[i]] <- new_df
    freq_df[[i]] <- freq_df[[i]][,-2]
  }
  return(freq_df)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------