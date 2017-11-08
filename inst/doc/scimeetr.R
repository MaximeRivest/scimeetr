## ---- include=FALSE------------------------------------------------------
library(scimeetr)

## ---- eval=FALSE---------------------------------------------------------
#  library(scimeetr)
#  scimeetr_list <- import_wos_files("path/to/folder/")

## ------------------------------------------------------------------------
summary(scimeetr_list)

## ---- error=FALSE--------------------------------------------------------
scimap_result <- scimap(scimeetr_list)

## ------------------------------------------------------------------------
summary(scimap_result)

## ---- fig.height = 7, fig.width=7----------------------------------------
plot(summary(scimap_result, com_size = 30))

## ---- eval=FALSE---------------------------------------------------------
#  reading_list <- scilist(scimap_result)
#  reading_list$com1

## ---- echo = FALSE-------------------------------------------------------
reading_list <- scilist(scimap_result)
knitr::kable(reading_list$com1)

