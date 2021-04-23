#' Produce list of book chapters
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame with published book chapters, from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' published book chapters
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' published book chapters; for example, a quadrennium like c(2017, 2020), which
#' is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' lbchapters <- listbchapters(lattes_data,
#'                             quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lbchapters <- listbchapters(lattes_folder_data,
#'                             quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listbchapters <- function(lattesdata,
                          quadre = NULL) {

  l <- length(lattesdata[["book_chapter"]])
  if (l == 0) {
    stop("There are no listed book chapters in any of the parsed Lattes xml files")
  }

  # Adding the complete DOI link
  # Loop over each cell to replace blank cells with NA
  lattesdata$book_chapter[] <- lapply(lattesdata$book_chapter, gsub, pattern = "^$", replacement = NA)
  na <- !is.na(lattesdata$book_chapter$DOI)
  lattesdata$book_chapter$DOI[na] <- paste0("https://doi.org/", lattesdata$book_chapter$DOI[na])

  bchapters <- lattesdata[["book_chapter"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(bchapters)[1] != "NOME") {
    bchapters <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                            bchapters)
  }

  delcols <- grepl("TIPO|NATUREZA|MEIO-DE-DIVULGACAO|HOME-PAGE-DO-TRABALHO|FLAG-RELEVANCIA|TITULO-DO-CAPITULO-DO-LIVRO-INGLES|FLAG-DIVULGACAO-CIENTIFICA|INFORMACOES.ADICIONAIS|SEQUENCIA.PRODUCAO|NUMERO-DE-VOLUMES|NUMERO-DA-EDICAO-REVISAO|NUMERO-DA-SERIE|PALAVRAS.CHAVE|AREAS.DO.CONHECIMENTO|NOME.COMPLETO.DO.AUTOR.[[:digit:]]|ORDEM.DE.AUTORIA.[[:digit:]]|SETORES.DE.ATIVIDADE.SETOR.DE.ATIVIDADE.[[:digit:]]|PALAVRAS.CHAVE.PALAVRA.CHAVE.[[:digit:]]|NOME.COMPLETO.DO.AUTOR.[[:digit:]]",
                   names(bchapters))
  bchapters <- bchapters %>% select(names(bchapters)[!delcols])

  names(bchapters) <- toupper(names(bchapters))
  names(bchapters) <- gsub("[-]|\\s", ".", names(bchapters))
  names(bchapters) <- gsub("[.]DE[.]|[.]DA[.]|[.]DO[.]|[.]DOS[.]", ".", names(bchapters))

  bchapters$ANO <- gsub("^\\s|\\s$", "", bchapters$ANO)
  bchapters <- bchapters %>% arrange(NOME, desc(ANO))

  # This for loop below corrects book chapters with just one author, so as to not
  # count as if it tagged as last author as well
  s <- bchapters[,"PRIMEIRO.AUTOR"] == "Sim"
  bchapters$ULTIMO.AUTOR[s] <- ifelse(bchapters$PRIMEIRO.AUTOR[s] == bchapters$ULTIMO.AUTOR[s],
                                      "Nao",
                                      bchapters$ULTIMO.AUTOR[s])

  # Defining a base year
  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    bchapters <- bchapters %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(bchapters)

}
