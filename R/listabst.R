#' Produce list of published abstracts in conferences
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing published abstracts in conferences, from
#' a tibble obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' published abstracts in conferences
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' published abstracts in conferences; for example, a quadrennium like c(2017, 2020),
#' which is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' labst <- listabst(lattes_data,
#'                   quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' labst <- listabst(lattes_folder_data,
#'                   quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listabst <- function (lattesdata,
                      quadre = NULL) {

  # Loop over each cell to replace blank cells with NA
  lattesdata$conference[] <- lapply(lattesdata$conference, gsub, pattern = "^$", replacement = NA)

  labst <- lattesdata[["conference"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(labst)[1] != "NOME") {
    labst <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                        labst)
  }

  labst <- labst %>% select("NOME",
                            "CLASSIFICACAO-DO-EVENTO",
                            "NATUREZA",
                            "ANO-DO-TRABALHO",
                            "TITULO-DO-TRABALHO",
                            "NOME-DO-EVENTO",
                            "TITULO-DOS-ANAIS-OU-PROCEEDINGS",
                            "PAIS-DO-EVENTO",
                            "CIDADE-DO-EVENTO",
                            "N autores",
                            "Posicao autoria",
                            "Primeiro autor",
                            "Ultimo autor")

  names(labst)[names(labst) == "ANO-DO-TRABALHO"] <- "ANO"
  names(labst) <- toupper(names(labst))
  names(labst) <- gsub("[-]|\\s", ".", names(labst))
  names(labst) <- gsub("[.]DE[.]|[.]DA[.]|[.]DO[.]|[.]DOS[.]", ".", names(labst))

  labst <- labst %>% arrange(NOME, CLASSIFICACAO.EVENTO, NATUREZA, desc(ANO))

  # Defining a base year
  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    labst <- labst %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(labst)

}
