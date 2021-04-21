#' Get list of papers
#'
#' @author Domingos Cardoso
#'
#' @description It obtains a dataframe with published and accepted papers from a tibble obtained using
#' get_lattes or get_lattes_folder. By proviving a capes file, the function adds
#' the classification of each journal by CAPES strata
#'
#' @param nupexfile table (tibble format) which will be used to extract
#' published and accepted papers as obtained by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}
#'
#' @param capesfile a dataframe of journal classification according to CAPES strata
#'
#' @param quadre a vector defing an interval of years to extract the list of papers
#'
#' @return A table in dataframe format.
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                      "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' data(capes_qualis)
#' lpapers <- listpapers(lattes_data
#'                       capesfile = capes_qualis,
#'                       quadre = c(2017, 2020))
#'}
#'
#' @importFrom plyr rbind.fill
#' @importFrom dplyr arrange filter select
#'
#' @export
#'

listpapers <- function(nupexfile,
                       capesfile = NULL,
                       quadre = NULL) {

  require(dplyr)
  # Adding the complete DOI link
  # Loop over each cell to replace blank cells with NA
  nupexfile$papers[] <- lapply(nupexfile$papers, gsub, pattern = "^$", replacement = NA)
  na <- !is.na(nupexfile$papers$DOI)
  nupexfile$papers$DOI[na] <- paste0("https://doi.org/", nupexfile$papers$DOI[na])

  papers_tempA <- nupexfile[["papers"]]
  papers_tempB <- nupexfile[["papers_accepted"]]
  papers_tempB$VOLUME <- "in press"

  delcols <- grepl("NATUREZA|PAIS-DE-PUBLICACAO|IDIOMA|MEIO-DE-DIVULGACAO|HOME-PAGE-DO-TRABALHO|FLAG-RELEVANCIA|TITULO-DO-ARTIGO-INGLES|SETORES.DE.ATIVIDADE|ORDEM.IMPORTANCIA|FLAG-DIVULGACAO-CIENTIFICA|PALAVRAS.CHAVE|AREAS.DO.CONHECIMENTO|FASCICULO|LOCAL-DE-PUBLICACAO|SEQUENCIA.PRODUCAO|INFORMACOES.ADICIONAIS.DESCRICAO|NOME.COMPLETO.DO.AUTOR.[[:digit:]]|ORDEM.DE.AUTORIA.[[:digit:]]",
                   names(papers_tempA))
  papers_tempA <- papers_tempA %>% select(names(papers_tempA)[!delcols])
  delcols <- grepl("NATUREZA|PAIS-DE-PUBLICACAO|IDIOMA|MEIO-DE-DIVULGACAO|HOME-PAGE-DO-TRABALHO|FLAG-RELEVANCIA|TITULO-DO-ARTIGO-INGLES|SETORES.DE.ATIVIDADE|ORDEM.IMPORTANCIA|FLAG-DIVULGACAO-CIENTIFICA|PALAVRAS.CHAVE|AREAS.DO.CONHECIMENTO|FASCICULO|LOCAL-DE-PUBLICACAO|SEQUENCIA.PRODUCAO|INFORMACOES.ADICIONAIS.DESCRICAO|NOME.COMPLETO.DO.AUTOR.[[:digit:]]|ORDEM.DE.AUTORIA.[[:digit:]]",
                   names(papers_tempB))
  papers_tempB <- papers_tempB %>% select(names(papers_tempB)[!delcols])

  papers_temp <- plyr::rbind.fill(papers_tempA, papers_tempB)

  names(papers_temp)[names(papers_temp) == "ANO-DO-ARTIGO"] <- "ANO"
  names(papers_temp) <- toupper(names(papers_temp))
  names(papers_temp) <- gsub("[-]|\\s", ".", names(papers_temp))
  names(papers_temp) <- gsub("[.]DE[.]|[.]DA[.]|[.]DO[.]|[.]DOS[.]", ".", names(papers_temp))
  papers_temp$ANO <- gsub("^\\s|\\s$", "", papers_temp$ANO)
  papers <- papers_temp %>% arrange(NOME, desc(ANO))
  #names(papers)[names(papers) == "TITULO.PERIODICO.OU.REVISTA"] = "TITULO_DO_PERIODICO"

  # Make manual/specific adjustments in ISSN
  papers <- adjust_ISSN(papers)

  # Make specific changes on the names of the journal as well as adding
  # the classification of each journal according to CAPES.
  # To do this a capes file (spreadsheet with classification of each jounral) has to be added
  if (!is.null(capesfile)) {

    # Correecting journal names and classification according to capes file
    a <- papers$ISSN_temp %in% capesfile$Print_ISSN
    aa <- is.na(papers$ISSN_temp[a])
    papers$ISSN_temp[a][!aa]

    for(i in papers$ISSN_temp[a][!aa]){
      papers$TITULO.PERIODICO.OU.REVISTA <- ifelse(papers$ISSN_temp == i,
                                                      as.character(capesfile$TITULO[which(capesfile$Print_ISSN == i)]),
                                                      as.character(papers$TITULO.PERIODICO.OU.REVISTA))
      papers$ESTRATO <- ifelse(papers$ISSN_temp == i,
                               as.character(capesfile$ESTRATO[which(capesfile$Print_ISSN == i)]),
                               as.character(papers$ESTRATO))
    }

    na <- is.na(papers$TITULO.PERIODICO.OU.REVISTA)
    nab <- is.na(papers$ISSN_temp[na])
    papers$ISSN_temp[na][!nab]

    b <- papers$ISSN_temp[na][!nab] %in% capesfile$E_ISSN

    for(i in papers$ISSN_temp[na][!nab][b]){
      papers$TITULO.PERIODICO.OU.REVISTA <- ifelse(papers$ISSN_temp == i,
                                                      as.character(capesfile$TITULO[which(capesfile$E_ISSN == i)]),
                                                      as.character(papers$TITULO.PERIODICO.OU.REVISTA))
      papers$ESTRATO <- ifelse(papers$ISSN_temp == i,
                               as.character(capesfile$ESTRATO[which(capesfile$E_ISSN == i)]),
                               as.character(papers$ESTRATO))
    }

    # Inserting remaining journal names based on the CAPES-provided ISSN
    # but that are not in the Scopus database (this is the temp columns added
    # to capesfile in a nother script)
    na <- is.na(papers$TITULO.PERIODICO.OU.REVISTA)

    nab <- is.na(papers$ISSN_temp[na])
    papers$ISSN_temp[na][!nab]

    b <- papers$ISSN_temp[na][!nab] %in% capesfile$ISSN_temp

    for(i in papers$ISSN_temp[na][!nab][b]){
      papers$TITULO.PERIODICO.OU.REVISTA <- ifelse(papers$ISSN_temp == i,
                                                      as.character(capesfile$TITULO[which(capesfile$ISSN_temp == i)]),
                                                      as.character(papers$TITULO.PERIODICO.OU.REVISTA))
      papers$ESTRATO <- ifelse(papers$ISSN_temp == i,
                               as.character(capesfile$ESTRATO[which(capesfile$ISSN_temp == i)]),
                               as.character(papers$ESTRATO))
    }

    # Some cleaning in the journal titles
    names(papers) <- gsub("TITULO[.]PERIODICO[.]OU[.]REVISTA", "TITULO-DO-PERIODICO-OU-REVISTA", names(papers))
    papers <- adjust_journ(papers)
    papers <- papers[[1]]
    names(papers) <- gsub("TITULO[-]DO[-]PERIODICO[-]OU[-]REVISTA", "TITULO.PERIODICO.OU.REVISTA", names(papers))

  }

  # Inserting back the names of journals after ISSN adjustments, either with
  # the adjust_ISSN function or after running the cleaning with capesfile
  na <- is.na(papers$TITULO.PERIODICO.OU.REVISTA)
  papers$TITULO.PERIODICO.OU.REVISTA[na] <- papers$periodico_temp[na]


  # This for loop below correct papers with just one author, so as to not count
  # as if it tagged as last author as well
  s <- papers[,"PRIMEIRO.AUTOR"] == "Sim"
  papers$ULTIMO.AUTOR[s] <- ifelse(papers$PRIMEIRO.AUTOR[s] == papers$ULTIMO.AUTOR[s],
                                   "Nao",
                                   papers$ULTIMO.AUTOR[s])


  papers <- papers %>% select(-c("periodico_temp","ISSN_temp"))


  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    papers <- papers %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  # Remove columns from dataframe where ALL values are NA
  papers <- papers[, colSums(is.na(papers)) < nrow(papers)]


  return(papers)

}
