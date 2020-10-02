get_supervision_ongoing <- function(xmls_list) {
  paper <- xmls_list$`DADOS-COMPLEMENTARES`$`ORIENTACOES-EM-ANDAMENTO`
  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    detalhes <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      pre <- data.frame(as.list(unlist(paper[[i]])))
      remover_nomes <- paste0(unique(names(paper[[i]])), ".")
      remover_nomes2 <- gsub("-", ".", remover_nomes)
      for (j in 1:length(remover_nomes)) {
        colnames(pre) <- gsub(remover_nomes[j], "", colnames(pre))
        colnames(pre) <- gsub(remover_nomes2[j], "", colnames(pre))
      }
      
      names(pre)[names(pre) == "TIPO.DE.ORIENTACAO.CONCLUIDA"] <- "TIPO.DE.ORIENTACAO"
      names(pre) <- gsub(".attrs.", "", names(pre))
      names(pre) <- gsub(".PARA.MESTRADO", "", names(pre))
      names(pre) <- gsub(".DE.MESTRADO", "", names(pre))
      names(pre) <- gsub(".DE.DOUTORADO", "", names(pre))
      names(pre) <- gsub(".PARA.DOUTORADO", "", names(pre))
      names(pre) <- gsub(".PARA.POS.DOUTORADO", "", names(pre))
      names(pre) <- gsub(".DE.POS.DOUTORADO", "", names(pre))
      names(pre) <- gsub(".DE.INICIACAO.CIENTIFICA", "", names(pre))
      names(pre) <- gsub("OUTRAS.ORIENTACOES", "ORIENTACOES", names(pre))
      names(pre) <- gsub("TIPO.DE.ORIENTACAO.CONCLUIDA", "TIPO.DE.ORIENTACAO", names(pre))
      detalhes[[i]] <-  pre
    }
    resultado <- do.call(plyr::rbind.fill, detalhes)
    return(tibble::as_tibble(resultado))
  }
}

