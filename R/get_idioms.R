get_idioms <- function(xmls_list) {
  
  paper <- xmls_list$`DADOS-GERAIS`$IDIOMAS
  
  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    detalhes <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      pre <- tibble::as_tibble(as.list(paper[[i]]))
      detalhes[[i]] <-  pre
    }
    resultado <- do.call(plyr::rbind.fill, detalhes)
    return(tibble::as_tibble(resultado))
  }
}

