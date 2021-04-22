#' Produce total projects or grants
#'
#' @author Domingos Cardoso
#'
#' @description It uses the list of projects (data frame format) derived by
#' \code{\link{listproj}} so as to produce a summary of total projects,
#' as well as discriminanting the number of grants in each of a set of main
#' Brazilian and International funding agencies (e.g. CNPq, CAPES, FAPs, MMA,
#' Fiocruz, NSF, NERC, Royal Society, WHO, FAO, etc)
#'
#' @param projdata table (data frame format) derived by \code{\link{listproj}}
#' function, which has to be used so as to get the total number of granted projects
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}
#'
#' @param collabs vector containing the complete names of collaborators for the
#' Graduate Program; the provided names have to be exactly as shown in their
#' Lattes profiles; the non-listed collaborators will automatically have their
#' "VINCULO" defined as "Permanente"
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' lproj <- listproj(lattes_data,
#'                   quadre = c(2017, 2020))
#' tproj <- totalproj(projdata = lproj,
#'                    lattes_data)
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lproj <- listproj(lattes_folder_data,
#'                   quadre = c(2017, 2020))
#' collaborators <- c("Suzana Telles da Cunha Lima",
#'                    "Cláudia Dias de Santana")
#' tproj <- totalproj(projdata = lproj,
#'                    lattes_folder_data,
#'                    collabs = collaborators)
#'}
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#'
#' @export
#'

totalproj <- function(projdata,
                      lattesdata,
                      collabs = NULL) {

  tproj <- listID(lattesdata,
                  collabs = collabs)

  # Create new columns using all names from a vector
  newcols <- c("projPESQUISA", "projEXTENSAO", "projENSINO", "projDESENVOLVIMENTO",
               "projANDAMENTO", "projCONCLUIDO", "projDESATIVADO", "COORDENADOR",
               "CNPq", "CAPES", "FINEP", "MCTI", "MMA", "MS", "MEC",
               "FAPEAL", "FAPESB", "FACEPE", "FAPEMA", "FAPESQ", "FAPEPI", "FAPERN", "FAPITEC", "FAPESP", "FAPEMIG", "FAPES", "FAPERJ", "FAPERGS", "FAPESC", "FA", "FAPEAM", "FAPEAP", "FAPESPA", "FAPERO", "FAPERR", "FUNDECT", "FAPEG", "FAPEMAT", "FAPT",
               "UFBA", "UFMG", "UEFS",
               "Fiocruz", "ICMBio", "EMBRAPA", "SENAI",
               "Petrobras", "O Boticario", "Instituto Serrapilheira", "VALE",
               "NSF", "NERC", "Kew", "Royal Society", "WWF", "National Geographic Society",
               "NIH", "Novo Nordisk Foundation",
               "European Commission", "ERC", "WHO", "FAO")

  tproj[, newcols] <- NA
  tproj <- as.data.frame(tproj)

  for (i in seq_along(unique(tproj$NOME))) {

    g <- tproj$NOME %in% unique(projdata$NOME)

    if (any(g[i])) {

      projdata_temp <- projdata %>% filter(NOME == tproj$NOME[i])

      p <- names(table(projdata_temp["NATUREZA"])) == "PESQUISA"
      if (any(p)) {
        tproj$projPESQUISA[i] <- as.vector(table(projdata_temp["NATUREZA"])[p])
      }
      p <- names(table(projdata_temp["NATUREZA"])) == "EXTENSAO"
      if (any(p)) {
        tproj$projEXTENSAO[i] <- as.vector(table(projdata_temp["NATUREZA"])[p])
      }
      p <- names(table(projdata_temp["NATUREZA"])) == "ENSINO"
      if (any(p)) {
        tproj$projENSINO[i] <- as.vector(table(projdata_temp["NATUREZA"])[p])
      }
      p <- names(table(projdata_temp["NATUREZA"])) == "DESENVOLVIMENTO"
      if (any(p)) {
        tproj$projDESENVOLVIMENTO[i] <- as.vector(table(projdata_temp["NATUREZA"])[p])
      }

      p <- names(table(projdata_temp["SITUACAO"])) == "EM_ANDAMENTO"
      if (any(p)) {
        tproj$projANDAMENTO[i] <- as.vector(table(projdata_temp["SITUACAO"])[p])
      }
      p <- names(table(projdata_temp["SITUACAO"])) == "CONCLUIDO"
      if (any(p)) {
        tproj$projCONCLUIDO[i] <- as.vector(table(projdata_temp["SITUACAO"])[p])
      }
      p <- names(table(projdata_temp["SITUACAO"])) == "DESATIVADO"
      if (any(p)) {
        tproj$projDESATIVADO[i] <- as.vector(table(projdata_temp["SITUACAO"])[p])
      }

      p <- names(table(projdata_temp["FLAG.RESPONSAVEL"])) == "SIM"
      if (any(p)) {
        tproj$COORDENADOR[i] <- as.vector(table(projdata_temp["FLAG.RESPONSAVEL"])[p])
      }

      # Filling in the number of funding (grants and scholarship: AUXILIO FINACEIRO and BOLSA)
      # from each specific agency
      for (inst in newcols[9:length(newcols)]) {
        n <- sapply(inst, grepl,
                    projdata_temp[, -c(1:11, length(projdata_temp))])
        if (any(n)) {

          total_inst <- agencies(inst, projdata_temp)

          if (any(unlist(total_inst))) {

            tproj[i, inst] <- length(which(unlist(total_inst) == T))
          }
        }
      }
    }
  }

  # Remove columns from dataframe where ALL values are NA
  tproj <- tproj[, colSums(is.na(tproj)) < nrow(tproj)]

  # # Fill in all NA by 0
  # ori <- as.data.frame(ori)
  # for(i in 4:length(names(ori))){
  #   ori[,i][is.na(ori[,i])] <- 0
  # }

  return(tproj)

}


# Auxiliary function to get the number of funding agencies
agencies <- function (x, y) {

  y <- y[, -c(1:11, length(y))]

  g <- sapply(x, grepl, y)

  res <- list()
  for (i in seq_along(names(y)[g])) {

    res[[i]] <- y[, names(y)[g][i]] %in% x
  }
  return(res)
}
