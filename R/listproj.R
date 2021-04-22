#' Produce list of projects or grants
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing the projects or grants, from a tibble
#' obtained using \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' projects or grants
#'
#' @param quadre a vector defining a time frame of years to extract the list of
#' projects or grants; for example, a quadrennium like c(2017, 2020), which is
#' useful for CAPES Sucupira report
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' lproj <- listproj(lattes_data
#'                   quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lproj <- listproj(lattes_folder_data,
#'                   quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listproj <- function(lattesdata,
                     quadre = NULL) {

  l <- length(lattesdata[["projects"]])
  if (l == 0) {
    stop("There are no listed projects or grants in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$projects[] <- lapply(lattesdata$projects, gsub, pattern = "^$", replacement = NA)

  lproj <- as.data.frame(lattesdata[["projects"]])

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(lproj)[1] != "NOME") {
    lproj <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                        lproj)
  }

  lequipe <- grepl("EQUIPE.DO.PROJETO.INTEGRANTES.DO.PROJETO.NOME.COMPLETO", names(lproj))
  #names(lproj)[which(lequipe == T)]

  t <- vector()
  for (i in seq_along(lproj$NOME)) {
    for (j in which(lequipe == T)) {
      t[j] <- is.na(lproj[i,j])
    }
    lproj$TOTAL.INTEGRANTES[i] <- length(which(t == F))
  }

  aa <- grepl("FINANCIADORES.DO.PROJETO.FINANCIADOR.DO.PROJETO.NATUREZA|FINANCIADORES.DO.PROJETO.FINANCIADOR.DO.PROJETO.NOME.INSTITUICAO",
              names(lproj))

  newcols <- c("NOME", "NATUREZA", "SITUACAO", "ANO.INICIO", "ANO.FIM",
               "EQUIPE.DO.PROJETO.INTEGRANTES.DO.PROJETO.FLAG.RESPONSAVEL", "NOME.DO.PROJETO",
               "TOTAL.INTEGRANTES", "NUMERO.GRADUACAO", "NUMERO.MESTRADO.ACADEMICO",
               "NUMERO.DOUTORADO", names(lproj)[aa], "DESCRICAO.DO.PROJETO")
  lproj <- lproj %>% select(all_of(newcols))
  names(lproj)[names(lproj) == "EQUIPE.DO.PROJETO.INTEGRANTES.DO.PROJETO.FLAG.RESPONSAVEL"] <- "FLAG.RESPONSAVEL"

  names(lproj) <- gsub("FINANCIADORES[.]DO[.]PROJETO[.]", "", names(lproj))
  names(lproj) <- gsub("FINANCIADOR[.]DO[.]PROJETO[.]NOME[.]INSTITUICAO$",
                       "PRINCIPAL.FINANCIADOR.DO.PROJETO.NOME.INSTITUICAO",
                       names(lproj))
  names(lproj) <- gsub("FINANCIADOR[.]DO[.]PROJETO[.]NATUREZA$",
                       "PRINCIPAL.FINANCIADOR.DO.PROJETO.NATUREZA",
                       names(lproj))

  names(lproj) <- gsub("[.]DE[.]|[.]DA[.]|[.]DO[.]", ".", names(lproj))

  lproj <- lproj %>% arrange(NOME, desc(NATUREZA), desc(SITUACAO), ANO.INICIO)

  if (!is.null(quadre)) {
    baseyear <- quadre[1]
    lastyear <- quadre[2]
    teste <- lproj %>% filter(ANO.INICIO >= baseyear & ANO.INICIO <= lastyear |
                                ANO.FIM >= baseyear & ANO.FIM <= lastyear |
                                ANO.INICIO < baseyear & is.na(ANO.FIM))
  }

  # Remove columns from data frame where ALL values are NA
  lproj <- lproj[, colSums(is.na(lproj)) < nrow(lproj)]

  # Using the auxiliary functions coop and replace_coop for deleting columns
  # related to COOPERACAO
  nt <- sapply("COOPERACAO",
               grepl, lproj[, -c(1:11, length(lproj))])
  if (any(nt)) {
    ag <- coop("COOPERACAO", lproj)
    lproj <- replace_coop("COOPERACAO", nt, ag, lproj)
  }

  # Remove again columns from dataframe where ALL values are NA
  lproj <- lproj[, colSums(is.na(lproj)) < nrow(lproj)]

  # Renumbering the final remaining columns
  nat <- grepl("NATUREZA", names(lproj[, -c(1:13, length(lproj))]))
  l <- names(lproj[, -c(1:13, length(lproj))])[nat]
  for (i in seq_along(l)) {
    names(lproj) <- gsub(colnames(lproj[, -c(1:13, length(lproj))])[nat][i],
                         paste0("FINANCIADOR.PROJETO.NATUREZA.", i),
                         names(lproj))
  }
  inst <- grepl("INSTITUICAO", names(lproj[, -c(1:13, length(lproj))]))
  l <- names(lproj[, -c(1:13, length(lproj))])[inst]
  for (i in seq_along(l)) {
    names(lproj) <- gsub(colnames(lproj[, -c(1:13, length(lproj))])[inst][i],
                         paste0("FINANCIADOR.PROJETO.NOME.INSTITUICAO.", i),
                         names(lproj))
  }

  # Standardizing the names of Institutions and Agencies
  lproj <- adjust_inst(lproj)

  return(lproj)

}


# Auxiliary functions for deleting columns related to COOPERAÇÃO
# These functions will look at COOPERACAO only within columns related
# to "FINANCIADOR.PROJETO" and will put NA in both "NATUREZA" and "NOME.INSTITUICAO"

coop <- function(x, y) {
  y <- y[, -c(1:11, length(y))]
  g <- sapply(x, grepl, y)
  res <- list()
  for (i in seq_along(names(y)[g])) {
    res[[i]] <- y[, names(y)[g][i]] %in% x
  }
  return(res)
}

replace_coop <- function(x, nt, ag, lproj) {
  nf <- which(nt == T) - 1
  if (length(ag) == 1) {
    lproj[, -c(1:11, length(lproj))][, nt][ag[[1]]] <- NA
    lproj[, -c(1:11, length(lproj))][, nf][ag[[1]]] <- NA
  } else {
    for (i in 1:length(ag)) {
      lproj[, -c(1:11, length(lproj))][, nt][i][ag[[i]], ] <- NA
      lproj[, -c(1:11, length(lproj))][, nf][i][ag[[i]], ] <- NA
    }
  }
  return(lproj)
}
