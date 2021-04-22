#' Produce list of technical works
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing the technical works, from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param nupexfile table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' technical works
#'
#' @param quadre a vector defining a time frame of years to extract the list of
#' technical works; for example, a quadrennium like c(2017, 2020), which is useful
#' for CAPES Sucupira report
#'
#' @return Table in dataframe format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' ltech <- listtech(lattes_data,
#'                   quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' ltech <- listtech(lattes_folder_data,
#'                   quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listtech <- function(nupexfile,
                     quadre = NULL) {

  l <- length(nupexfile[["tech_work"]])
  if (l == 0) {
    stop("There are no listed technical works in any of the parsed Lattes xml files")
  }

  nupexfile$tech_work[] <- lapply(nupexfile$tech_work, gsub, pattern = "^$", replacement = NA)

  ltech <- nupexfile[["tech_work"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(ltech)[1] != "NOME") {
    ltech <- data.frame(NOME = nupexfile$basic$`NOME-COMPLETO`,
                        ltech)
  }

  ltech <- ltech %>% select("NOME", "NATUREZA", "ANO",
                            "TITULO.DO.TRABALHO.TECNICO",
                            "FINALIDADE")

  names(ltech) <- gsub("[.]DO[.]", ".", names(ltech))

  ltech <- ltech %>% arrange(NOME, NATUREZA, desc(ANO))

  # Defining a base year
  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    ltech <- ltech %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(ltech)

}
