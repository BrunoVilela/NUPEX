#' Produce list of supervisions
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing ongoing and finished supervisions,
#' from a tibble obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param nupexfile table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract the list of
#' supervisions
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' supervisions; for example, a quadrennium like c(2017, 2020), which is useful
#' for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' lsuperv <- listsuperv(lattes_data,
#'                       quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lsuperv <- listsuperv(lattes_folder_data,
#'                       quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select mutate_all
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listsuperv <- function(nupexfile,
                       quadre = NULL) {

  # Loop over each cell to replace blank cells with NA
  nupexfile$supervision[] <- lapply(nupexfile$supervision, gsub, pattern = "^$", replacement = NA)
  nupexfile$supervision_ongoing[] <- lapply(nupexfile$supervision_ongoing, gsub, pattern = "^$", replacement = NA)

  superv <- nupexfile[["supervision"]]
  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(superv)[1] != "NOME") {
    superv <- data.frame(NOME = nupexfile$basic$`NOME-COMPLETO`,
                         superv)
  }
  superv <- superv %>% mutate_all(as.character) %>%
    select("NOME","NATUREZA","TITULO","ANO",
           "TIPO.DE.ORIENTACAO","NOME.DO.ORIENTADO",
           "NOME.DA.INSTITUICAO","NOME.DO.CURSO","CODIGO.CURSO","FLAG.BOLSA",
           "NOME.DA.AGENCIA","PAIS")

  ongoingsuperv <- nupexfile[["supervision_ongoing"]]
  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(ongoingsuperv)[1] != "NOME") {
    ongoingsuperv <- data.frame(NOME = nupexfile$basic$`NOME-COMPLETO`,
                                ongoingsuperv)
  }
  ongoingsuperv <- ongoingsuperv %>% mutate_all(as.character) %>%
    select("NOME","NATUREZA","TITULO.DO.TRABALHO", "ANO",
           "TIPO.DE.ORIENTACAO","NOME.DO.ORIENTANDO",
           "NOME.INSTITUICAO","NOME.CURSO","CODIGO.CURSO","FLAG.BOLSA",
           "NOME.DA.AGENCIA","PAIS")

  names(ongoingsuperv) <- names(superv)

  superv$STATUS <- "CONCLUIDA"
  ongoingsuperv$STATUS <- "EM ANDAMENTO"
  lsuperv <- rbind(superv,ongoingsuperv)

  lsuperv$NATUREZA[lsuperv$NATUREZA == "TRABALHO_DE_CONCLUSAO_DE_CURSO_GRADUACAO"] <- "Trabalho de conclusão de curso de graduação"
  lsuperv$NATUREZA[lsuperv$NATUREZA == "INICIACAO_CIENTIFICA"] <- "Iniciação Científica"
  lsuperv$NATUREZA[lsuperv$NATUREZA == "ORIENTACAO-DE-OUTRA-NATUREZA"] <- "Orientação de outra natureza"
  lsuperv$NATUREZA[lsuperv$NATUREZA == "MONOGRAFIA_DE_CONCLUSAO_DE_CURSO_APERFEICOAMENTO_E_ESPECIALIZACAO"] <- "Monografia de conclusão de curso de aperfeiçoamento especialização"

  names(lsuperv) <- gsub("[.]DE[.]|[.]DA[.]|[.]DO[.]", ".", names(lsuperv))

  # Abreviating names of funding agencies by using just acronyms
  lsuperv <- adjust_ag(lsuperv)

  lsuperv <- lsuperv %>% arrange(NOME, desc(STATUS), NATUREZA, desc(ANO)) %>%
    select("NOME","NATUREZA","ANO","STATUS","TITULO",
           "TIPO.ORIENTACAO","NOME.ORIENTADO",
           "NOME.INSTITUICAO","NOME.CURSO","CODIGO.CURSO","FLAG.BOLSA",
           "NOME.AGENCIA","PAIS")

  # Adjusting names of PPGs
  lsuperv <- adjust_ppgs(lsuperv)

  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    lsuperv <- lsuperv %>% filter(ANO >= baseyear & ANO <= lastyear)
  }

  return(lsuperv)

}
