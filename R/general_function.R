.general_func <- function(paper, xmls_list) {
  if (is.null(paper)) {
    return(NULL)
  } else {
    n_papers <- length(paper)
    resultado <- list()
    resto <- list()
    for (i in 1:n_papers) {
      # Detalhe do paper
      detalhes <- unlist(paper[[i]][1:2])
      # Autoria
      autores <- names(paper[[i]]) == "AUTORES"
      autor <- NULL
      nome <- xmls_list$`DADOS-GERAIS`$.attrs[1]
      n_autores <- sum(autores)
      for (j in which(autores)) {
        if (paper[[i]][[j]][1] == nome) {
          posicao_autoria <- paper[[i]][[j]][3]
        }
      }
      resultado[[i]] <- as.data.frame(as.list(c(detalhes, n_autores, posicao_autoria,
                                                ifelse(1 == posicao_autoria, "Sim", "Nao"),
                                                ifelse(n_autores == posicao_autoria, "Sim", "Nao"))))
      colnames(resultado[[i]]) <- c(names(detalhes), "N autores", "Posicao autoria",
                                    "Primeiro autor",
                                    "Ultimo autor")
      resto[[i]] <- data.frame(as.list(unlist(paper[[i]][-(1:2)])))

    }
    resultado <- do.call(plyr::rbind.fill, resultado)
    resultado2 <- do.call(plyr::rbind.fill, resto)
    resultado_final <- tibble::as_tibble(cbind(resultado, resultado2))
    remover_nomes <- paste0(unique(names(paper[[1]])), ".")
    for (i in 1:length(remover_nomes)) {
      colnames(resultado_final) <- gsub(remover_nomes[i], "",
                                        colnames(resultado_final))
    }
    return(resultado_final)
  }
}
