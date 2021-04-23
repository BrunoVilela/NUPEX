#' Produce list of registered softwares
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame with registered softwares from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' list of published patents
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' registered softwares; for example, a quadrennium like c(2017, 2020), which is
#' useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' lsoft <- listsoft(lattes_data
#'                   quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lsoft <- listsoft(lattes_folder_data,
#'                   quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#' @importFrom plyr rbind.fill
#'
#' @export
#'

listsoft <- function(lattesdata,
                     quadre = NULL) {

  l <- length(lattesdata[["software"]])
  if (l == 0) {
    stop("There are no listed registered sofwares in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$software[] <- lapply(lattesdata$software, gsub, pattern = "^$", replacement = NA)

  lsoft <- lattesdata[["software"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(lsoft)[1] != "NOME") {
    lsoft <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                        lsoft)
  }

  lsoft <- lsoft %>% select("NOME",
                            "NATUREZA",
                            "REGISTRO.OU.PATENTE.TIPO.PATENTE",
                            "ANO",
                            "TITULO.DO.SOFTWARE",
                            "PLATAFORMA",
                            "AMBIENTE",
                            "FINALIDADE",
                            "PAIS",
                            "REGISTRO.OU.PATENTE.CODIGO.DO.REGISTRO.OU.PATENTE",
                            "REGISTRO.OU.PATENTE.INSTITUICAO.DEPOSITO.REGISTRO")

  colnames(lsoft)[colnames(lsoft) == "TITULO.DO.SOFTWARE"] <- "TITULO.SOFTWARE"
  colnames(lsoft)[colnames(lsoft) == "REGISTRO.OU.PATENTE.CODIGO.DO.REGISTRO.OU.PATENTE"] <- "CODIGO.PATENTE"
  colnames(lsoft)[colnames(lsoft) == "REGISTRO.OU.PATENTE.INSTITUICAO.DEPOSITO.REGISTRO"] <- "INSTITUICAO.DEPOSITO.REGISTRO"
  colnames(lsoft)[colnames(lsoft) == "REGISTRO.OU.PATENTE.TIPO.PATENTE"] <- "TIPO.PATENTE"

  lsoft <- lsoft %>% arrange(NOME, desc(ANO), NATUREZA)

  # Defining a base year
  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    lsoft <- lsoft %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(lsoft)

}
