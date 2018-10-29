#' Summary scimeetr
#'
#' @param object a scimeetr object
#' @param com_size minimum size of a community to plot
#' @param ... options to plot
#' @return object of class scimeetrSum
#' @method summary scimeetr
#' @export
#' @import purrr dplyr

summary.scimeetr <- function(object, com_size = 200, ...){
  lsci <- object
  lev_com <- stringr::str_count(names(lsci), '_')
  lparent_com <- lsci[[which(min(lev_com) == lev_com)]]
  nb_papers <- nrow(lparent_com$dfsci)
  nb_ref <- length(unique(toupper(unlist(stringr::str_split(lparent_com$dfsci$CR, '; ')))))
  avg_nb_ref <- round(mean(purrr::map_int(stringr::str_split(lparent_com$dfsci$CR, '; '), length), na.rm = T))
  quant_nb_citation <- quantile(lparent_com$dfsci$TC, na.rm = T)
  mean_nb_citation <- mean(lparent_com$dfsci$TC, na.rm = T)
  pub_age <- as.integer(stringr::str_extract(Sys.Date(), '^[0-9]{4}'))-lparent_com$dfsci$PY # find age of publication
  avg_nb_citation_yr <- mean(lparent_com$dfsci$TC/pub_age, na.rm = T, trim = Inf)
  lparent_com$dfsci$DE[which(lparent_com$dfsci$DE == "")] <- NA
  top10kw <- sort(table(toupper(unlist(stringr::str_split(lparent_com$dfsci$DE, '; ')))),
                 decreasing = T)[1:10]
  class(top10kw) <- "table"
  top10so <- sort(table(toupper(lparent_com$dfsci$SO)),
                  decreasing = T)[1:10]
  class(top10so) <- "table"
  ltag <- map(lsci, "tag") %>%
    discard(is_empty) %>%
    map(function(x) c('',x)) %>%
    bind_rows() %>%
    as.data.frame() %>%
    tidyr::gather(comID, tag)%>%
    mutate(comID = if_else(tag == '', comID,''))
  # Make step printing
  st <- which(ltag$comID != "")
  end <- st + 6
  n_cols <- max(lev_com) + 1
  if(n_cols >= 2) {
    for(i in 1:(n_cols - 1)) {
      ltag <- cbind(ltag, ltag$tag)
    }
    for(i in st){
      col_depth <- stringr::str_count(ltag[i,1], '_') + 1
      if((col_depth + 2)>ncol(ltag)) {
        ltag[(i+1):(i+6), 1:col_depth] <- ""
      } else {
        ltag[(i+1):(i+6),c(1:col_depth, (col_depth + 2):ncol(ltag))] <- ""
      }

    }
  }


  com_size <- map(lsci, 'dfsci') %>% map_int(nrow)
  ltag$comID[ltag$comID %in% names(com_size)] <- paste0(names(com_size), ' (', com_size, ')')
  # Prepare igraph object for community summary plot
  # need to make a from and to edgelist
  levs <- sort(unique(lev_com))[1:length(unique(lev_com))-1]
  el_com <- NULL
  for(i in levs){
    t <- lsci[which(lev_com == i)]
    for(j in 1:length(t)){
      j_lsci <- t[j]
      if(any(stringr::str_detect(names(lsci), paste0(names(j_lsci), "_[0-9]*$")))){
        el_com <- rbind(el_com,
                        data.frame(from = names(j_lsci),
                                   to = names(lsci)[stringr::str_detect(names(lsci), paste0(names(j_lsci), "_[0-9]*$"))],
                                   stringsAsFactors = F))
      }
    }
  }
  # node attribute df
  node_com <- map(lsci, "tag") %>%
    map(paste, collapse = ";\n ") %>%
    bind_rows() %>%
    as.data.frame() %>%
    tidyr::gather(comID, tag)
  node_com$size <- map(lsci, 'dfsci') %>%
    map_int(nrow)
  node_com$cr <- transpose(lsci)$cr %>%
    map(function(x){
      x <- x[1:25,]
      if(any(names(x)=='Relative_frequency')){
        x <- filter(x, "Relative_frequency" >1.05) %>%
          select(ID)
        x <- x$ID[1:6]
      } else {
        x <- x$ID[1:6]
      }
      return(x)
    }) %>%
    map_chr(function(x)paste0(x, collapse=';\n'))
  node_com$so <- transpose(lsci)$dfsci %>%
    map('SO') %>%
    map(table) %>%
    map(sort, decreasing = T) %>%
    map_chr(function(x)paste0(names(x[1:6]), collapse=';\n'))
  node_com$ji <- transpose(lsci)$dfsci %>%
    map('JI') %>%
    map(table) %>%
    map(sort, decreasing = T) %>%
    map_chr(function(x)paste0(names(x[1:6]), collapse=';\n'))
  node_com$au <- transpose(lsci)$dfsci %>%
    map('AU') %>%
    map(stringr::str_split, '; ') %>%
    map(unlist) %>%
    map(table) %>%
    map(sort, decreasing = T) %>%
    map_chr(function(x)paste0(names(x[1:6]), collapse=';\n'))
  node_com$ab <- transpose(lsci)$ab %>%
    map(function(x){
      x <- x[1:25,]
      if(any(names(x)=='Relative_frequency')){
        x <- filter(x, "Relative_frequency" >1.05) %>%
          select(ID)
        x <- x$ID[1:6]
      } else {
        x <- x$ID[1:6]
      }
      return(x)
    }) %>%
    map_chr(function(x)paste0(x, collapse=';\n'))
  node_com$ti <- transpose(lsci)$ti %>%
    map(function(x){
      x <- x[1:25,]
      if(any(names(x)=='Relative_frequency')){
        x <- filter(x, "Relative_frequency" >1.05) %>%
          select(ID)
        x <- x$ID[1:6]
      } else {
        x <- x$ID[1:6]
      }
      return(x)
    }) %>%
    map_chr(function(x)paste0(x, collapse=';\n'))

  if(!is.null(el_com)) {
    net <- igraph::graph_from_data_frame(d=el_com, vertices=node_com, directed=T)
    subnet <- igraph::induced_subgraph(net, igraph::V(net)[size>=com_size])
    lsum <- list(nb_papers = nb_papers,
                 nb_ref = nb_ref,
                 avg_nb_ref = avg_nb_ref,
                 quant_nb_citation = quant_nb_citation,
                 mean_nb_citation = mean_nb_citation,
                 avg_nb_citation_yr = avg_nb_citation_yr,
                 top10kw = top10kw,
                 top10so = top10so,
                 ltag = ltag,
                 plot = subnet)
  } else {
    lsum <- list(nb_papers = nb_papers,
                 nb_ref = nb_ref,
                 avg_nb_ref = avg_nb_ref,
                 quant_nb_citation = quant_nb_citation,
                 mean_nb_citation = mean_nb_citation,
                 avg_nb_citation_yr = avg_nb_citation_yr,
                 top10kw = top10kw,
                 top10so = top10so,
                 ltag = ltag)
  }
  class(lsum) <- c("scimeetrsum", class(lsum))
  return(lsum)
}
