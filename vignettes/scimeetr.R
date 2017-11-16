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

## ---- eval=FALSE---------------------------------------------------------
#  scimeetr_list <- import_wos_files(files_directory = "/path/to/folder/")
#  scimeetr_list <- import_scopus_files(files_directory = "/path/to/folder/")

## ------------------------------------------------------------------------
scimeetr_list
summary(scimeetr_list)

## ---- eval=FALSE---------------------------------------------------------
#  scimeetr_list$com1$dfsci

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(subset(scimeetr_list$com1$dfsci[1,],select = c(-AB, -CR, -FX)), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, k = 3)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, k = 3), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "core_yr", k = 2)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "core_yr", k = 2), format = "markdown")

## ---- message=FALSE, echo=FALSE------------------------------------------
splt_cr <- split_cr(scimeetr_list) 
cr_df <- dplyr::inner_join(splt_cr, scimeetr_list$com1$cr, by = 'ID')
library(dplyr, quietly = T)
x <- cr_df %>%
      mutate(age=as.integer(stringr::str_extract(Sys.Date(), '^[0-9]{4}')) - as.integer(as.character(cr_df$year))) %>%
      filter(age <= 40 & age > 2) %>% 
      group_by(age) %>%
      top_n(n = 10, wt = Frequency.x) %>%
      select(record, Frequency.x, age) %>%
      arrange(age, desc(Frequency.x)) %>%
      ungroup()
library(ggplot2)
ggplot(x, aes(x = age, y = Frequency.x)) +
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x, k=10), se = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "core_residual", k = 3)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "core_residual", k = 3), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "by_expert_LC", k = 2, m = 2)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "by_expert_LC", k = 2, m = 2), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "group_of_experts_LC", k = 5)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "group_of_experts_LC", k = 5), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "betweeness", k = 5)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "betweeness", k = 5), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "closeness", k = 5)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "closeness", k = 5), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "connectness", k = 5)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "connectness", k = 5), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "page_rank", k = 5)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "page_rank", k = 5), format = "markdown")

## ---- eval=FALSE---------------------------------------------------------
#  scilist(scimeetr_list, reading_list = "cite_most_others", k = 5)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(scilist(scimeetr_list, reading_list = "cite_most_others", k = 5), format = "markdown")

## ------------------------------------------------------------------------
summary(scimap(scimeetr_list, coupling_by = 'bic', community_algorithm = 'louvain', min_com_size = 100))
summary(scimap(scimeetr_list, coupling_by = 'abc', community_algorithm = 'louvain', min_com_size = 100))
summary(scimap(scimeetr_list, coupling_by = 'tic', community_algorithm = 'louvain', min_com_size = 100))
summary(scimap(scimeetr_list, coupling_by = 'kec', community_algorithm = 'louvain', min_com_size = 100))

## ------------------------------------------------------------------------
scil <- scimap(scimeetr_list)
scil
subscil <- focus_on(scil, grab = 'com1_1')
subscil

## ------------------------------------------------------------------------
scil <- scimap(scimap(scimeetr_list))
scil
subscil <- dive_to(scil, aim_at = 'com1_1')
subscil

