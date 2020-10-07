#' Summary all lattes information
#'
#' It summarizes the data obtained from get_lattes_folder
#'
#' @param lattes_data output result from get_lattes_folder function.
#'
#' @return A summary list
#'
#' @examples
#' folder_path <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(folder_path)
#' summary_lattes <- summary_all(lattes_folder_data)
#' @export
#' @importFrom magrittr "%>%"
#'


summary_all <- function(lattes_data) {
  N <- length(lattes_data$basic$NOME)
  c(
  summary_basic(lattes_data$basic),
  summary_education(lattes_data$education),
  summary_courses(lattes_data$courses, N),
  summary_address(lattes_data$address),
  summary_idioms(lattes_data$idiom, N),
  summary_areas(lattes_data$areas),
  summary_papers(lattes_data$papers, N),
  summary_papers_accepted(lattes_data$papers_accepted, N),
  summary_conference(lattes_data$conference, N),
  summary_books(lattes_data$books, N),
  summary_book_chapter(lattes_data$book_chapter, N),
  summary_text_media(lattes_data$text_media, N),
  summary_other_pubs(lattes_data$other_pubs, N),
  summary_supervision(lattes_data$supervision, N),
  summary_supervision(lattes_data$supervision_ongoing, N),
  summary_defense_tribunal(lattes_data$defense_tribunal, N),
  summary_other_comissions(lattes_data$other_comissions, N),
  summary_events_participation(lattes_data$events_participation, N),
  summary_prizes(lattes_data$prizes, N),
  summary_tech_prod(lattes_data$tech_prod, N),
  summary_software(lattes_data$software, N),
  summary_patent(lattes_data$patent, N),
  summary_tech_work(lattes_data$tech_work, N),
  summary_projects(lattes_data$projects, N),
  summary_journal_referee(lattes_data$journal_referee, N),
  summary_journal_editor(lattes_data$journal_editor, N)
  )
}



#----------------------------------- BASIC
summary_basic <- function(x) {
  N_pessoas <- length(unique(x$`NOME-COMPLETO`))
  N_paises <- length(unique(x$`PAIS-DE-NASCIMENTO`))
  N_estados <- length(unique(x$`UF-NASCIMENTO`))
  N_cidades <- length(paste0(unique(x$`UF-NASCIMENTO`), "-",
                             unique(x$`CIDADE-NASCIMENTO`)))
  return(list("Number of people" = N_pessoas,
              "Number of countries" = N_paises,
              "Number of states" = N_estados,
              "Number of cities" = N_cidades))
}


#----------------------------------- EDUCATION
summary_education <- function(x) {
  suppressMessages(NIVEL_max <- x %>%
                     dplyr::group_by(NOME) %>%
                     dplyr::filter(!NIVEL %in% c("X", "C", 6:100)) %>%
                     dplyr::summarize(nivel = max(NIVEL)))
  NIVEL_max$nivel[NIVEL_max$nivel == "5"] <- "Postdoc"
  NIVEL_max$nivel[NIVEL_max$nivel == "4"] <- "PhD"
  NIVEL_max$nivel[NIVEL_max$nivel == "3"] <- "Master"
  NIVEL_max$nivel[NIVEL_max$nivel == "2"] <- "Specialization"
  NIVEL_max$nivel[NIVEL_max$nivel == "1"] <- "Undergrad"
  resultado <- table(NIVEL_max$nivel)
  return(list("Highest education level" = resultado))
}

#----------------------------------- COURSES
summary_courses <- function(x, N) {
  if (!is.null(x)) {
    suppressMessages(NIVEL_max <- x %>%
                       dplyr::group_by(NOME) %>%
                       dplyr::summarize(N.CURSOS = length(NIVEL),
                                        CARGA.HORARIA.TOTAL = sum(as.numeric(CARGA.HORARIA))))
    cursos_total <- sum(as.numeric(NIVEL_max$N.CURSOS))
    cursos_media <- cursos_total / N
    CH <- sum(as.numeric(NIVEL_max$CARGA.HORARIA.TOTAL), na.rm = TRUE)
    CH_media <- CH / N } else {
      cursos_media <- 0
      CH_media <- 0
    }
  return(list("Mean courses per person" = cursos_media,
              "Mean of the total time in hours spend in courses per person" = CH_media))

}



#----------------------------------- COURSES
summary_address <- function(x) {
  n_uni <- length(unique(x$ENDERECO.PROFISSIONAL.CODIGO.INSTITUICAO.EMPRESA))
  n_dept <- length(unique(x$ENDERECO.PROFISSIONAL.NOME.ORGAO))
  return(list("Number of universities" = n_uni,
              "Number of departaments" = n_dept))
}


#----------------------------------- IDIOMS
summary_idioms <- function(x, N) {
  N_total <- length(unique(x$IDIOMA))
  suppressMessages(NIVEL_max <- x %>%
                     dplyr::group_by(NOME) %>%
                     dplyr::summarize(N.CURSOS = length(IDIOMA)))
  N_medio <- sum(NIVEL_max$N.CURSOS) / N
  return(list("Total number of unique languages" = N_total,
              "Mean number of languages per person" = N_medio))
}


#----------------------------------- Areas
summary_areas <- function(x) {
  N_fields <- length(unique(x$NOME.DA.AREA.DO.CONHECIMENTO))
  return(list("Number of research fields" = N_fields))
}

#----------------------------------- papers
summary_papers <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO-DO-ARTIGO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique paper" = N_artigos,
              "Mean number of paper per person" = mean_artigos))
}

#----------------------------------- papers accepted
summary_papers_accepted <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO-DO-ARTIGO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique accepted paper" = N_artigos,
              "Mean number of accepted paper per person" = mean_artigos))
}

#----------------------------------- conference
summary_conference <- function(x, N) {
  if (!is.null(x)) {
    N_eventos <- length(unique(x$`NOME-DO-EVENTO`))
    N_pais <- length(unique(x$`PAIS-DO-EVENTO`))
    N_artigos <- length(unique(x$`TITULO-DO-TRABALHO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
    N_eventos <- 0
    N_pais <- 0
  }
  return(list("Total number of unique conference works" = N_artigos,
              "Mean number of conference works per person" = mean_artigos,
              "Total number of unique conferences" = N_eventos,
              "Total number of countries hosting the conferences" = N_pais))
}

#----------------------------------- conference
summary_books <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO-DO-LIVRO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique books" = N_artigos,
              "Mean number of books per person" = mean_artigos))
}



#----------------------------------- book chapter
summary_book_chapter <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO-DO-CAPITULO-DO-LIVRO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique book chapters" = N_artigos,
              "Mean number of books chapters per person" = mean_artigos))
}


#----------------------------------- text media
summary_text_media <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO-DO-TEXTO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique texts in different media" = N_artigos,
              "Mean number of texts in different media per person" = mean_artigos))
}

#----------------------------------- other publications
summary_other_pubs <- function(x, N) {
  if (!is.null(x)) {
    trads <- is.na(x$`DETALHAMENTO-DA-TRADUCAO.TITULO-DA-OBRA-ORIGINAL`)
    if (sum(trads != 0)) {
    N_artigos <- length(unique(x$`TITULO`[trads]))
    mean_artigos <- nrow(x[trads, ]) / N
    } else {
      N_artigos <- 0
      mean_artigos <- 0
    }
    if (sum(!trads) != 0) {
      N_trad <- length(unique(x[!trads, ]$`DETALHAMENTO-DA-TRADUCAO.TITULO-DA-OBRA-ORIGINAL`))
      mean_trad <- sum(!trads) /  N
    } else {
      N_trad <- 0
      mean_trad <- 0
    }
  } else {
    N_artigos <- 0
    mean_artigos <- 0
    N_trad <- 0
    mean_trad <- 0
  }
  return(list("Total number of unique other publications" = N_artigos,
              "Mean number of other publications per person" = mean_artigos,
              "Total number of unique translations" = N_trad,
              "Mean number of translations per person" = mean_trad))
}


#----------------------------------- supervision
summary_supervision <- function(x, N) {
  if (!is.null(x)) {
    titulo <- "Supervision summary"
    if ("TITULO.DO.TRABALHO" %in% colnames(x)) {
      colnames(x)[colnames(x) == "TITULO.DO.TRABALHO"] <- "TITULO"
      titulo <- "Supervision ongoing summary"
    }
    sumario <- x %>%
      dplyr::group_by(NATUREZA, TIPO.DE.ORIENTACAO) %>%
      dplyr::summarise(TOTAL = length(TITULO),
                       `MEDIA POR PESSOA` = TOTAL / N) %>%
      dplyr::mutate(NATUREZA = stringr::str_to_title(gsub("_", " ", NATUREZA)),
                    TIPO.DE.ORIENTACAO = stringr::str_to_title(gsub("_", "-", TIPO.DE.ORIENTACAO)),
                    TIPO.DE.ORIENTACAO = ifelse(is.na(TIPO.DE.ORIENTACAO), "", TIPO.DE.ORIENTACAO))

  } else {
    sumario <- NULL
  }
  resultado <- list(sumario)
  names(resultado) <- titulo
  return(resultado)
}

#----------------------------------- defense_tribunal
summary_defense_tribunal <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique defense tribunals" = N_artigos,
              "Mean number of texts in defense tribunals participation per person" = mean_artigos))
}


#----------------------------------- defense tribunal
summary_defense_tribunal <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$`TITULO`))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique defense tribunals" = N_artigos,
              "Mean number of texts in defense tribunals participation per person" = mean_artigos))
}

#----------------------------------- other comissions
summary_other_comissions <- function(x, N) {
  if (!is.null(x)) {
    sumario <- x %>%
      dplyr::group_by(NATUREZA) %>%
      dplyr::summarise(TOTAL = length(TITULO),
                       `MEDIA POR PESSOA` = TOTAL / N)

  } else {
    sumario <- NULL
  }
  return(list("Comissions summary" = sumario))
}

#----------------------------------- Prizes
summary_events_participation <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$NOME.DO.EVENTO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique events" = N_artigos,
              "Mean number of events participation per person" = mean_artigos))
}



#----------------------------------- Prizes
summary_prizes <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- nrow(x)
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of prizes" = N_artigos,
              "Mean number of prizes per person" = mean_artigos))
}

#----------------------------------- Technical products
summary_tech_prod <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$TITULO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique technical products" = N_artigos,
              "Mean number of technical products per person" = mean_artigos))
}

#----------------------------------- Software
summary_software <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$TITULO.DO.SOFTWARE))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique software" = N_artigos,
              "Mean number of software per person" = mean_artigos))
}
#----------------------------------- Patent
summary_patent <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$TITULO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique software" = N_artigos,
              "Mean number of software per person" = mean_artigos))
}

#----------------------------------- Technical work
summary_tech_work <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$TITULO.DO.TRABALHO.TECNICO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique technical work" = N_artigos,
              "Mean number of technical work per person" = mean_artigos))
}

#----------------------------------- Projects
summary_projects <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$NOME.DO.PROJETO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique projects" = N_artigos,
              "Mean number of projects per person" = mean_artigos))
}

#----------------------------------- Journal referee
summary_journal_referee <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$NOME.INSTITUICAO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique journals for which papers were reviewed" = N_artigos,
              "Mean number of journals for which papers were reviewed per person" = mean_artigos))
}

#----------------------------------- Journal editor
summary_journal_editor <- function(x, N) {
  if (!is.null(x)) {
    N_artigos <- length(unique(x$NOME.INSTITUICAO))
    mean_artigos <- nrow(x) / N
  } else {
    N_artigos <- 0
    mean_artigos <- 0
  }
  return(list("Total number of unique journals in which people acted as editor" = N_artigos,
              "Mean number of journals in which people acted as editor" = mean_artigos))
}
