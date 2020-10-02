
get_courses <- function(xmls_list) {
  paper <- xmls_list$`DADOS-COMPLEMENTARES`$`FORMACAO-COMPLEMENTAR`

  if (is.null(paper)) {
    return(NULL)
  } else {
    resultado <- .complementar(paper)
    paper2 <- xmls_list$`DADOS-COMPLEMENTARES`$`INFORMACOES-ADICIONAIS-CURSOS`
    resultado2 <- .complementar(paper2)
    resultado <- suppressMessages(dplyr::left_join(resultado, resultado2))
    paper3 <- xmls_list$`DADOS-COMPLEMENTARES`$`INFORMACOES-ADICIONAIS-INSTITUICOES`
    resultado3 <- .complementar(paper3)
    resultado <- suppressMessages(dplyr::left_join(resultado, resultado3))
    return(resultado)
  }
}

.complementar <- function(paper) {
  n_papers <- length(paper)
  detalhes <- list()
  for (i in 1:n_papers) {
    # Detalhe do paper
    detalhes[[i]] <- data.frame(as.list(unlist(paper[[i]])))
  }
  resultado <- do.call(plyr::rbind.fill, detalhes)
  colnames(resultado) <- gsub(".attrs.", "", colnames(resultado))
  resultado <- tibble::as_tibble(resultado)
  return(resultado)
}
