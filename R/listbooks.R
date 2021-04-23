#' Produce list of books
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing the published books, from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' published books
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' published books; for example, a quadrennium like c(2017, 2020), which is useful
#' for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' lbooks <- listbooks(lattes_data,
#'                     quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lbooks <- listbooks(lattes_folder_data,
#'                     quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listbooks <- function(lattesdata,
                      quadre = NULL) {

  l <- length(lattesdata[["books"]])
  if (l == 0) {
    stop("There are no listed books in any of the parsed Lattes xml files")
  }

  # Adding the complete DOI link
  # Loop over each cell to replace blank cells with NA
  lattesdata$books[] <- lapply(lattesdata$books, gsub, pattern = "^$", replacement = NA)
  na <- !is.na(lattesdata$books$DOI)
  lattesdata$books$DOI[na] <- paste0("https://doi.org/", lattesdata$books$DOI[na])

  books <- lattesdata[["books"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(books)[1] != "NOME") {
    books <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                        books)
  }

  delcols <- grepl("TIPO|NATUREZA|MEIO-DE-DIVULGACAO|HOME-PAGE-DO-TRABALHO|FLAG-RELEVANCIA|TITULO-DO-LIVRO-INGLES|FLAG-DIVULGACAO-CIENTIFICA|INFORMACOES.ADICIONAIS|SEQUENCIA.PRODUCAO|NUMERO-DE-VOLUMES|NUMERO-DA-EDICAO-REVISAO|NUMERO-DA-SERIE|PALAVRAS.CHAVE|AREAS.DO.CONHECIMENTO|NOME.COMPLETO.DO.AUTOR.[[:digit:]]|ORDEM.DE.AUTORIA.[[:digit:]]|SETORES.DE.ATIVIDADE.SETOR.DE.ATIVIDADE.[[:digit:]]|PALAVRAS.CHAVE.PALAVRA.CHAVE.[[:digit:]]",
                   names(books))
  books <- books %>% select(names(books)[!delcols])

  names(books) <- toupper(names(books))
  names(books) <- gsub("[-]|\\s", ".", names(books))
  names(books) <- gsub("[.]DE[.]|[.]DA[.]|[.]DO[.]|[.]DOS[.]", ".", names(books))

  books$ANO <- gsub("^\\s|\\s$", "", books$ANO)
  books <- books %>% arrange(NOME, desc(ANO))

  # This for loop below correct books with just one author, so as to not count
  # as if it tagged as last author as well
  s <- books[,"PRIMEIRO.AUTOR"] == "Sim"
  books$ULTIMO.AUTOR[s] <- ifelse(books$PRIMEIRO.AUTOR[s] == books$ULTIMO.AUTOR[s],
                                  "Nao",
                                  books$ULTIMO.AUTOR[s])

  # Defining a base year
  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    books <- books %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(books)

}
