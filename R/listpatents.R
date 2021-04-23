#' Produce list of patents
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame with published patents from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' list of published patents
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' patents; for example, a quadrennium like c(2017, 2020), which is useful for
#' CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' lpats <- listpatents(lattes_data
#'                      quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lpats <- listpatents(lattes_folder_data,
#'                      quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#' @importFrom plyr rbind.fill
#'
#' @export
#'

listpatents <- function(lattesdata,
                        quadre = NULL) {

  l <- length(lattesdata[["patent"]])
  if (l == 0) {
    stop("There are no listed patents in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$patent[] <- lapply(lattesdata$patent, gsub, pattern = "^$", replacement = NA)

  lpats <- lattesdata[["patent"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(lpats)[1] != "NOME") {
    lpats <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                        lpats)
  }

  lpats <- lpats %>% select("NOME",
                            "CATEGORIA",
                            "ANO.DESENVOLVIMENTO",
                            "TITULO",
                            "FINALIDADE",
                            "PAIS",
                            "REGISTRO.OU.PATENTE.TIPO.PATENTE",
                            "REGISTRO.OU.PATENTE.CODIGO.DO.REGISTRO.OU.PATENTE",
                            "REGISTRO.OU.PATENTE.INSTITUICAO.DEPOSITO.REGISTRO")

  colnames(lpats)[colnames(lpats) == "REGISTRO.OU.PATENTE.CODIGO.DO.REGISTRO.OU.PATENTE"] <- "CODIGO.PATENTE"
  colnames(lpats)[colnames(lpats) == "REGISTRO.OU.PATENTE.INSTITUICAO.DEPOSITO.REGISTRO"] <- "INSTITUICAO.DEPOSITO.REGISTRO"
  colnames(lpats)[colnames(lpats) == "ANO.DESENVOLVIMENTO"] <- "ANO"

  lpats <- lpats %>% arrange(NOME, desc(ANO), CATEGORIA)

  # Defining a base year
  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    lpats <- lpats %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(lpats)

}
