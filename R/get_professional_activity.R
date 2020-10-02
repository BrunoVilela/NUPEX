get_professional_activity <- function(xmls_list) {

  paper <- xmls_list$`DADOS-GERAIS`$`ATUACOES-PROFISSIONAIS`

  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    detalhes <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      pre <- data.frame(as.list(unlist(paper[[i]])))
      detalhes[[i]] <-  pre
    }
    resultado <- do.call(plyr::rbind.fill, detalhes)
    remover_nomes <- paste0(unique(names(paper[[1]])), ".")
    for (i in 1:length(remover_nomes)) {
      colnames(resultado) <- gsub(remover_nomes[i], "", colnames(resultado))
    }
    retornar <- suppressMessages(tibble::as_tibble(resultado,
                                           .name_repair = "universal"))
    return(retornar)
  }
}

