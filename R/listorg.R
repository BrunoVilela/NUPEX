#' Produce list of contributions with conference organizing committee
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing contributions with conference
#' organizing committee, from a tibble obtained using \code{\link{get_lattes}}
#' or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' of contributions with conference organizing committee
#'
#' @param quadre a vector defining a time frame of years to extract the list of
#' contributions with conference organizing committee; for example, a quadrennium
#' like c(2017, 2020), which is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' lorg <- listorg(lattes_data
#'                 quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lorg <- listorg(lattes_data,
#'                 quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listorg <- function (lattesdata,
                     quadre = NULL) {

  l <- length(lattesdata[["tech_prod"]])
  if (l == 0) {
    stop("There are no listed organized conferences in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$tech_prod[] <- lapply(lattesdata$tech_prod, gsub, pattern = "^$", replacement = NA)

  lorg <- lattesdata[["tech_prod"]]

  if (names(lorg)[1] != "NOME") {
    lorg <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                       lorg)
  }

  lorg <- lorg %>% filter(NATUREZA == "ORGANIZACAO") %>%
    select("NOME", "TIPO", "ANO", "TITULO", "INSTITUICAO.PROMOTORA", "PAIS", "CIDADE")

  lorg <- lorg %>% arrange(NOME, TIPO, desc(ANO))

  # Defining a base year
  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    lorg <- lorg %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(lorg)

}
