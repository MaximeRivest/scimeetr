automatic_by_com_html <- 
  function(suit_freq_tables, table_length){
    table_type <- c(
"Table of most prolific authors",
"Table of most cited articles", 
"Table of most published in journals",
"Table of most published in journals",
"Table of most published in years",
"Table of most assigned to categories",
"Table of most assigned to research area",
"Table of most mentionned keywords",
"Table of most prolific universities",
"Table of most prolific countries")
    page_wrap_head <- rep(c("<div id=\"page-wrap\">", "", ""), ceiling(length(suit_freq_tables$CR) / 3))[1:length(suit_freq_tables$CR)]
    page_wrap_tail <- rep(c("", "", "</div>"), ceiling(length(suit_freq_tables$CR) / 3))[1:length(suit_freq_tables$CR)]
    order_of_tables <- c(8,7,6,3,2,1,9,10,5)
    final_core_html <- NULL

for (current_community in 1:length(suit_freq_tables$CR)) {
  page_body <- NULL
  for (current_table_type in order_of_tables) {
    page_body <- append(page_body,paste(
"    
<div class=\"post\">
                  
```{r, echo = FALSE, message=FALSE, warning=FALSE, results='asis'}
library(pander)
panderOptions('table.split.table', Inf)
set.caption(", deparse(table_type[current_table_type]),")
temp_table <- ", deparse(substitute(suit_freq_tables)), "[[",deparse(current_table_type),"]][[", deparse(as.numeric(current_community)), "]][1:", deparse(as.numeric(table_length)), ",]
pander(temp_table, style = \"simple\")
```
    
<a href=\"#header\">retour à la table des matières</a>
</div>
", sep = ""))
  }
  big_body <- paste(page_wrap_head, page_body, page_wrap_tail, sep = "" )
  big_body <- paste(big_body, collapse = "")
  if((length(suit_freq_tables$CR) / 3)%%1==0){
    final_core_html <- append(final_core_html, paste("\n###Community : ", deparse(as.numeric(current_community)), "\n", big_body,sep = ""))
    } else {final_core_html <- append(final_core_html, paste(table_type[current_table_type], big_body, "</div>", sep = ""))}
  }
  
    final_core_html <- paste(final_core_html, collapse = "")
    return(final_core_html)
  }

