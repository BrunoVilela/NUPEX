#' Produce list of prizes
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing prizes or award titles, from
#' a tibble obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' prizes or award titles
#'
#' @param quadre a vector defining a time frame of years to extract the list of
#' prizes or award titles; for example, a quadrennium like c(2017, 2020), which
#' is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' lprizes <- listprizes(lattes_data
#'                       quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lprizes <- listprizes(lattes_folder_data,
#'                       quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listprizes <- function (lattesdata,
                        quadre = NULL) {

  l <- length(lattesdata[["prizes"]])
  if (l == 0) {
    stop("There are no listed prizes or award titles in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$prizes[] <- lapply(lattesdata$prizes, gsub, pattern = "^$", replacement = NA)

  lprizes <- lattesdata[["prizes"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(lprizes)[1] != "NOME") {
    lprizes <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                          lprizes)
  }

  lprizes <- lprizes %>% select("NOME",
                                "NOME-DO-PREMIO-OU-TITULO",
                                "NOME-DA-ENTIDADE-PROMOTORA",
                                "ANO-DA-PREMIACAO")

  names(lprizes)[names(lprizes) == "ANO-DA-PREMIACAO"] <- "ANO"
  names(lprizes) <- gsub("[-]", ".", names(lprizes))
  names(lprizes) <- gsub("[.]DA[.]|[.]DO[.]", ".", names(lprizes))

  lprizes <- lprizes %>% arrange(NOME, desc(ANO))

  # Defining a base year
  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    lprizes <- lprizes %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(lprizes)

}
