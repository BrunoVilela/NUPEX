#' Produce list of professional activity in general committees
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing professional activity related to
#' general committees (e.g. funding agencies), from a tibble obtained using
#' \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' participation in general committees
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' participation in general committees; for example, a quadrennium like c(2017, 2020),
#' which is useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' lcomm <- listcomm(lattes_data,
#'                   quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lcomm <- listcomm(lattes_folder_data,
#'                   quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listcomm <- function (lattesdata,
                      quadre = NULL) {

  l <- length(lattesdata[["professional_activity"]])
  if (l == 0) {
    stop("There are no listed professional activity related to general committees
         in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$professional_activity[] <- lapply(lattesdata$professional_activity, gsub, pattern = "^$", replacement = NA)

  lcomm <- as.data.frame(lattesdata[["professional_activity"]])

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(lcomm)[1] != "NOME") {
    lcomm <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                        lcomm)
  }

  lcomm <- lattesdata[["professional_activity"]] %>%
    select("NOME", "OUTRO.VINCULO.INFORMADO", "NOME.INSTITUICAO", "ANO.INICIO", "ANO.FIM") %>%
    filter(OUTRO.VINCULO.INFORMADO == "Membro do Comitê de Avaliação" |
             OUTRO.VINCULO.INFORMADO == "Membro de comitê assessor" |
             OUTRO.VINCULO.INFORMADO == "Revisor de projeto de fomento" |
             OUTRO.VINCULO.INFORMADO == "Membro afiliado" |
             OUTRO.VINCULO.INFORMADO == "Membro associado" |
             OUTRO.VINCULO.INFORMADO == "Membro de Câmara" |
             OUTRO.VINCULO.INFORMADO == "Membro de Comitê" |
             OUTRO.VINCULO.INFORMADO == "Membro do Conselho" |
             OUTRO.VINCULO.INFORMADO == "Membro estrangeiro" |
             OUTRO.VINCULO.INFORMADO == "Presidente" |
             OUTRO.VINCULO.INFORMADO == "Pesquisador visitante" |
             OUTRO.VINCULO.INFORMADO == "Pesquisador Visitante" |
             OUTRO.VINCULO.INFORMADO == "Research Fellow" |
             OUTRO.VINCULO.INFORMADO == "Visiting Fellow")

  # Abreviating names of funding agencies by using just acronyms
  lcomm <- adjust_ag(lcomm)

  colnames(lcomm)[colnames(lcomm) == "OUTRO.VINCULO.INFORMADO"] <- "VINCULO"
  colnames(lcomm)[colnames(lcomm) == "NOME.AGENCIA"] <- "INSTITUICAO"

  lcomm <- lcomm %>% arrange(NOME, VINCULO, desc(ANO.INICIO))
  lcomm$ANO.FIM <- gsub("^$", NA, lcomm$ANO.FIM)

  # Defining a base year
  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    lcomm <- lcomm %>% filter(ANO.INICIO >= baseyear & ANO.INICIO <= lastyear |
                              ANO.FIM >= baseyear & ANO.FIM <= lastyear)
  }

  return(lcomm)

}
