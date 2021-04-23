#' Produce list of graduate programs where each one contributes with ongoing supervising
#'
#' @author Domingos Cardoso
#'
#' @description It uses a tibble obtained with \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}} to produce a data frame listing all graduate
#' programs where each one contributes with ongoing supervising of MSc or PhD students
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' ongoing supervisions of MSc or PhD students at different graduate programs
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
#' lattes_data <- get_lattes(path_lattes)
#' ppgs <- ppgsuperv(lattes_data)
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' collaborators <- c("Suzana Telles da Cunha Lima",
#'                    "Cláudia Dias de Santana")
#' ppgs <- ppgsuperv(lattes_folder_data,
#'                   collabs = collaborators)
#'}
#'
#' @importFrom dplyr arrange filter select mutate_all
#' @importFrom magrittr "%>%"
#' @importFrom plyr empty
#'
#' @export
#'

ppgsuperv <- function(lattesdata,
                      collabs = NULL) {

  IDs <- listID(lattesdata,
                collabs = collabs)

  ongosuperv <- lattesdata[["supervision_ongoing"]]

  ongosuperv <- ongosuperv %>% mutate_all(as.character) %>%
    filter(NATUREZA == "Tese de doutorado" | NATUREZA == "Dissertação de mestrado") %>%
    filter(TIPO.DE.ORIENTACAO == "ORIENTADOR_PRINCIPAL")

  # Adjusting the names of the ppgs and asscoiated codes
  ongosuperv <- adjust_ppgs(ongosuperv)

  ppgs_codes <- unique(ongosuperv$CODIGO.CURSO)
  ppgs <- data.frame(CODIGO.CURSO=ppgs_codes)

  ppgs$CODIGO.CURSO <- as.character(ppgs$CODIGO.CURSO)
  ppgs$NOME.INSTITUICAO <- NA
  ppgs$NOME.CURSO <- NA

  for (i in ppgs$CODIGO.CURSO) {
    ppgs$NOME.INSTITUICAO <- ifelse(ppgs$CODIGO.CURSO == i,
                                    paste(unique(ongosuperv$NOME.INSTITUICAO[ongosuperv$CODIGO.CURSO == i])),
                                    ppgs$NOME.INSTITUICAO)
    ppgs$NOME.CURSO <- ifelse(ppgs$CODIGO.CURSO == i,
                              paste(unique(ongosuperv$NOME.CURSO[ongosuperv$CODIGO.CURSO == i])),
                              ppgs$NOME.CURSO)

  }

  mest <- vector()
  dout <- vector()
  nomes <- list()
  for (i in IDs$NOME) {
    mest <- paste(i, "- Mestrado")
    dout <- paste(i, "- Doutorado")
    nomes[[i]] <- append(mest,dout)
  }
  supervisors <- as.vector(unlist(nomes))

  # create new columns using all names from a vector
  ppgs[,supervisors] <- NA

  for (i in supervisors){
    if (!gsub("\\s[-].+","",i) %in% ongosuperv$NOME) {
      ppgs[i] <- 0
    } else {
      for (j in ppgs$CODIGO.CURSO) {
        name_temp <- gsub("\\s[-].+","",i)

        temp <- ongosuperv$NATUREZA[ongosuperv$NOME == name_temp & ongosuperv$CODIGO.CURSO == j]

        if (gsub(".*\\s[-]\\s","",i) == "Mestrado") {
          mest <- as.vector(table(temp[temp == "Dissertação de mestrado"]))
          if (plyr::empty(data.frame(mest))) {
            ppgs[ppgs$CODIGO.CURSO == j, i] <- 0
          } else {
            ppgs[ppgs$CODIGO.CURSO == j, i] <- mest
          }
        }

        if (gsub(".*\\s[-]\\s","",i) == "Doutorado"){
          dout <- as.vector(table(temp[temp == "Tese de doutorado"]))
          if (plyr::empty(data.frame(dout))) {
            ppgs[ppgs$CODIGO.CURSO == j, i] <- 0
          } else {
            ppgs[ppgs$CODIGO.CURSO == j, i] <- dout
          }
        }

      }
    }
  }

  ppgs <- ppgs %>% arrange(NOME.INSTITUICAO, NOME.CURSO)

  return(ppgs)

}
