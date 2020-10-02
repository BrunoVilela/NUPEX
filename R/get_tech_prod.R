get_tech_prod <- function(xmls_list) {
  
  paper <- xmls_list$`PRODUCAO-TECNICA`$`DEMAIS-TIPOS-DE-PRODUCAO-TECNICA`
  
  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    detalhes <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      pre <- data.frame(as.list(c(paper[[i]][[1]],
                                  paper[[i]][[2]])))
      detalhes[[i]] <-  pre
    }
    resultado <- do.call(plyr::rbind.fill, detalhes)
    remover_nomes <- paste0(unique(names(paper[[1]])), ".")
    remover_nomes2 <- gsub("-", ".", remover_nomes)
    for (i in 1:length(remover_nomes)) {
      colnames(resultado) <- gsub(remover_nomes[i], "", colnames(resultado))
      colnames(resultado) <- gsub(remover_nomes2[i], "", colnames(resultado))
    }
    return(tibble::as_tibble(resultado))
  }
}

