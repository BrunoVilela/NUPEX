get_projects <- function(xmls_list) {
  paper <- xmls_list$`DADOS-GERAIS`$`ATUACOES-PROFISSIONAIS`
  if (is.null(paper)) {
    return(NULL)
  } else {
    qual <-
      which(sapply(xmls_list$`DADOS-GERAIS`$`ATUACOES-PROFISSIONAIS`,
                   function(x) {
                     "ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO"  %in%
                       names(x)
                   }))
    paper <- paper[qual]
    paper <-
      lapply(paper, function(x) {
        names(x) <- paste0(names(x), round(stats::runif(1), 2))
        x
      })


    if (is.null(paper) | length(paper) == 0) {
      return(NULL)
    } else {
      n_papers <- length(paper)
      detalhes <- list()
      count <- 0
      for (i in 1:n_papers) {
        # Detalhe do paper
        projetos <- paper[[i]]$`ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO`
        n_projetos <- length(projetos)
        for (j in 1:n_projetos) {
          n <- sum(names(projetos[[j]]) == "PROJETO-DE-PESQUISA")
          if (n > 0) {
            if (n == 1) {
              count <- count + 1
              pre0 <- unlist(projetos[[j]]$`PROJETO-DE-PESQUISA`)
              names(pre0) <- gsub(".attrs.", "", names(pre0))
              pre <- data.frame(as.list(pre0))
              detalhes[[count]] <-  pre
            } else {
              for (k in 1:n) {
                count <- count + 1
                pre0 <- unlist(projetos[[j]][[k]])
                names(pre0) <- gsub(".attrs.", "", names(pre0))
                pre <- data.frame(as.list(pre0))
                detalhes[[count]] <-  pre
              }
            }
          }
        }
      }
      resultado <- do.call(plyr::rbind.fill, detalhes)
      return(tibble::as_tibble(resultado))
    }
  }
}
