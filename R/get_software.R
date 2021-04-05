get_software <- function(xmls_list) {

  paper <- xmls_list$`PRODUCAO-TECNICA`
  paper <- paper[names(paper) == "SOFTWARE"]
  if (is.null(paper) | length(paper) == 0) {
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
    remover_nomes2 <- gsub("-", ".", remover_nomes)
    for (i in 1:length(remover_nomes)) {
      colnames(resultado) <- gsub(remover_nomes[i], "", colnames(resultado))
      colnames(resultado) <- gsub(remover_nomes2[i], "", colnames(resultado))
    }
    cols <- names(resultado)
    dups <- cols[duplicated(cols)]
    n <- length(dups)
    if (n != 0) {
      for (i in 1:n) {
        qual <- which(cols %in% dups[i])
        temp <- resultado[, qual]
        resultado[, qual[1]] <- ifelse(is.na(temp[, 1]), temp[, 2],
                                       temp[, 1])
      }
      resultado <- resultado[,!duplicated(cols)]
    }
    return(tibble::as_tibble(resultado))
  }
}

