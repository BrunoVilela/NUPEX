#' Produce list of participation in scientific events
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing the participation in scientific
#' events (e.g. meetings, congress, conferences), from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract the list of
#' participation in scientific events
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' participation in scientific events; for example, a quadrennium like c(2017, 2020),
#' which is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' levents <- listevents(lattes_data
#'                       quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' levents <- listevents(lattes_folder_data,
#'                       quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listevents <- function (lattesdata,
                        quadre = NULL) {

  # Loop over each cell to replace blank cells with NA
  lattesdata$events_participation[] <- lapply(lattesdata$events_participation, gsub,
                                              pattern = "^$", replacement = NA)

  levents <- lattesdata[["events_participation"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(levents)[1] != "NOME") {
    levents <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                          levents)
  }

  levents <- levents %>% select("NOME", "NATUREZA", "ANO", "TITULO", "NOME.DO.EVENTO",
                                "FORMA.PARTICIPACAO", "PAIS", "CIDADE.DO.EVENTO")

  names(levents) <- gsub("[.]DO[.]", ".", names(levents))

  levents <- levents %>% arrange(NOME, NATUREZA, desc(ANO), FORMA.PARTICIPACAO)

  # Defining a base year
  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    levents <- levents %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(levents)

}
