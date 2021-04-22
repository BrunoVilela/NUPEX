#' Produce list of participation in defense tribunals
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing participation in defense tribunals
#' (e.g. committees for evalution of thesis or undergraduate monographs), from
#' a tibble obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' participation in defense tribunals
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' participation in defense tribunals; for example, a quadrennium like c(2017, 2020),
#' which is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' ldefense <- listdefense(lattes_data
#'                         quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' ldefense <- listdefense(lattes_folder_data,
#'                         quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listdefense <- function (lattesdata,
                         quadre = NULL) {

  l <- length(lattesdata[["defense_tribunal"]])
  if (l == 0) {
    stop("There are no listed participation in defense tribunals in any of the
         parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$defense_tribunal[] <- lapply(lattesdata$defense_tribunal, gsub,
                                          pattern = "^$", replacement = NA)

  ldefense <- lattesdata[["defense_tribunal"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(ldefense)[1] != "NOME") {
    ldefense <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                           ldefense)
  }

  ldefense <- ldefense %>% select("NOME",
                                  "NATUREZA",
                                  "ANO",
                                  "TITULO",
                                  "PAIS",
                                  "NOME.DO.CANDIDATO",
                                  "NOME.INSTITUICAO",
                                  "NOME.CURSO",
                                  "CODIGO.CURSO")

  ldefense <- ldefense %>% arrange(NOME, NATUREZA, desc(ANO))
  names(ldefense) <- gsub("[.]DO[.]", ".", names(ldefense))

  # Adjusting names of PPGs
  ldefense <- adjust_ppgs(ldefense)

  # Defining a base year
  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    ldefense <- ldefense %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(ldefense)

}
