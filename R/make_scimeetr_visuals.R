make_scimeetr_visuals <- 
  function(WOS_dataframe, suit_freq_tables, table_length, tables_type_to_include){
    scimeetr_head <- paste(
"---
title:  \"Scientometric Meet R\"
author: \"Produced by the Package Scimeetr which is created by Maxime Rivest\"
date: ", Sys.Date(), "
output:
  html_document:
    keep_md: no
    number_section: yes
    theme: cosmo
    toc: yes
    toc_depth: 3
    css: ", paste(system.file("extdata", "style.css", package="scimeetr"), sep = ""),"
---
<head>
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js\"></script>
<script>
$(function(){
$(\"#page-wrap\").wrapInner(\"<table cellspacing='300'><tr>\");
$(\".post\").wrap(\"<td></td>\");
$(\"body\").mousewheel(function(event, delta) {
this.scrollLeft -= (delta * 30);\nevent.preventDefault();
});   
});
</script>
</head>

```{r , echo=FALSE, warning=FALSE, results='hide', message=FALSE}
library(knitr)

```
# Summary
### Number of records : `r nrow(", deparse(substitute(WOS_dataframe)), ")`
", sep = "")
    
  if(tables_type_to_include == "commumity"){
      scimeetr_body_com <- paste("\n## By community {.tabset}", automatic_by_com_html(suit_freq_tables, table_length = table_length), sep = "")
      scimeetr_html <- paste(scimeetr_head, scimeetr_body_com )
    }else if(tables_type_to_include == "table") {
      scimeetr_body_group <- paste("\n## By table type {.tabset}", automatic_by_group_html(suit_freq_tables, table_length = table_length), sep = "")
      scimeetr_html <- paste(scimeetr_head, scimeetr_body_group)
    }else if(tables_type_to_include == "all" ) {
      scimeetr_body_group <- paste("\n## By table type {.tabset}", automatic_by_group_html(suit_freq_tables, table_length = table_length), sep = "")
      scimeetr_body_com <- paste("\n## By community {.tabset}", automatic_by_com_html(suit_freq_tables, table_length = table_length), sep = "")
      scimeetr_html <- paste(scimeetr_head, scimeetr_body_group, scimeetr_body_com )
    }

  writeChar(scimeetr_html, con = "scimeetr.Rmd", eos = NULL)
  rmarkdown::render("scimeetr.Rmd", output_file = "scimeetr.html")
  browseURL(sprintf('file:///%s%s', file.path(getwd()), '/scimeetr.html')) 
  }
