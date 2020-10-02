get_conference_part <- function(xmls_list) {
  paper <- xmls_list$`DADOS-COMPLEMENTARES`$`PARTICIPACAO-EM-EVENTOS-CONGRESSOS`
  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    detalhes <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      pre <- data.frame(as.list(c(paper[[i]][[1]],
                                  paper[[i]][[2]])))
      names(pre)[names(pre) == "TIPO.DE.ORIENTACAO.CONCLUIDA"] <- "TIPO.DE.ORIENTACAO"
      detalhes[[i]] <-  pre
    }
    resultado <- do.call(plyr::rbind.fill, detalhes)
    return(tibble::as_tibble(resultado))
  }
}

