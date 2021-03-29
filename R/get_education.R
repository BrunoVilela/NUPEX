get_education <- function(xmls_list) {

  paper <- xmls_list$`DADOS-GERAIS`$`FORMACAO-ACADEMICA-TITULACAO`

  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    detalhes <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      if(".attrs" %in% names(paper[[i]])) {
      pre <- tibble::as_tibble(as.list(paper[[i]]$.attrs))
      } else {
        pre <- tibble::as_tibble(as.list(paper[[i]]))

      }
      names(pre) <- gsub("-DE-CONCLUSAO-DE-CURSO", "", names(pre))
      names(pre) <- gsub("DA-DISSERTACAO-TESE", "DO-TRABALHO", names(pre))
      names(pre) <- gsub("-GRAD", "", names(pre))
      names(pre) <- gsub("-DOUT", "", names(pre))



      detalhes[[i]] <-  pre
    }
    resultado <- do.call(plyr::rbind.fill, detalhes)
    if (!tibble::is_tibble(resultado)) {
      resultado <- tibble::as_tibble(resultado)
    }
    return(resultado)
  }
}

