#' Produce list of complete personal information
#'
#' @author Domingos Cardoso
#'
#' @description It uses a tibble obtained with \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}} to produce a data frame listing the complete
#' personal information (e.g. ORCID ID, Lattes ID, Nationality, institutional
#' address, PhD thesis), as publicily available in the
#' \href{http://lattes.cnpq.br/}{Plataforma Lattes}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' complete personal information
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
#' lcompID <- listcompID(lattes_data)
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' collaborators <- c("Suzana Telles da Cunha Lima",
#'                    "Cláudia Dias de Santana")
#' lcompIDs <- listcompID(lattes_folder_data,
#'                        collabs = collaborators)
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#' @importFrom plyr rbind.fill
#'
#' @export
#'

listcompID <- function (lattesdata,
                        collabs = NULL) {

  # Loop over each cell to replace blank cells with NA
  lattesdata$basic[] <- lapply(lattesdata$basic, gsub, pattern = "^$", replacement = NA)
  lcompID <- lattesdata[["basic"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(lcompID)[1] != "NOME") {
    lcompID <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                          lcompID)
  }

  names(lcompID) <- gsub("[-]", ".", names(lcompID))
  lcompID$ORCID.ID <- gsub("^$", NA, lcompID$ORCID.ID)
  names(lcompID)[names(lcompID) == "NUMERO.IDENTIFICADOR"] <- "LATTES.ID"

  lcompID$LATTES <- paste0("http://lattes.cnpq.br/", lcompID$LATTES.ID)

  lcompID <- lcompID %>% select("NOME",
                                "ORCID.ID",
                                "LATTES",
                                "LATTES.ID",
                                "DATA.ATUALIZACAO")

  if (!is.null(collabs)) {
    lcompID$VINCULO <- NA
    lcompID$VINCULO[lcompID$NOME %in% collabs] <- "Colaborador"
    lcompID$VINCULO[is.na(lcompID$VINCULO)] <- "Permanente"
  }

  addbasic <- lattesdata[["basic"]]
  if (names(addbasic)[1] != "NOME") {
    addbasic <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                           addbasic)
  }
  names(addbasic) <- gsub("[-]", ".", names(addbasic))
  addbasic <- addbasic %>% select("NOME",
                                  "PAIS.DE.NASCIMENTO",
                                  "SIGLA.PAIS.NACIONALIDADE",
                                  "CIDADE.NASCIMENTO")

  lattesdata$address[] <- lapply(lattesdata$address, gsub, pattern = "^$", replacement = NA)
  address <- lattesdata[["address"]]
  names(address) <- gsub("ENDERECO[.]PROFISSIONAL[.]|[.]EMPRESA|[.]COMPLEMENTO", "", names(address))

  if (names(address)[1] != "NOME") {
    address <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                          address)
  }

  address <- address %>% select("NOME",
                                "NOME.INSTITUICAO",
                                "NOME.ORGAO",
                                "LOGRADOURO",
                                "CEP",
                                "BAIRRO",
                                "UF",
                                "PAIS",
                                "DDD",
                                "TELEFONE",
                                "HOME.PAGE")

  lattesdata$education[] <- lapply(lattesdata$education, gsub, pattern = "^$", replacement = NA)
  edu <- lattesdata[["education"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(edu)[1] != "NOME") {
    edu <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                      edu)
  }

  edu <- edu %>% filter(NIVEL == "4")
  names(edu) <- gsub("[-]", ".", names(edu))

  dup <- duplicated(edu$NOME)
  if (any(dup)) {
    g <- edu$NOME %in% edu$NOME[dup][1]
    temp <- edu[g, ]
    temp <- temp %>% filter(ANO.DE.CONCLUSAO == max(temp$ANO.DE.CONCLUSAO))

    for (i in 2:length(edu$NOME[dup])) {
      g <- edu$NOME %in% edu$NOME[dup][i]
      addtemp <- edu[g, ]
      addtemp <- addtemp %>% filter(ANO.DE.CONCLUSAO == max(addtemp$ANO.DE.CONCLUSAO))

      temp <- rbind(temp, addtemp)
    }

    edu <- edu[!edu$NOME %in% edu$NOME[dup], ]
    edu <- rbind(edu, temp)
    edu <- edu %>% arrange(NOME)

    lcompID <- lcompID %>% arrange(NOME)
    addbasic <- addbasic %>% arrange(NOME)
    addbasic <- addbasic[, -1]
    address <- address %>% arrange(NOME)
    address <- address[, -1]
  } else {
    addbasic <- addbasic[, -1]
    address <- address[, -1]
  }

  edu <- edu %>% select("NOME",
                        "NOME.CURSO",
                        "NOME.INSTITUICAO",
                        "TITULO.DO.TRABALHO",
                        "ANO.DE.INICIO",
                        "ANO.DE.CONCLUSAO",
                        "NOME.COMPLETO.DO.ORIENTADOR",
                        "NUMERO.ID.ORIENTADOR")

  names(edu) <- paste0(names(edu), ".DOUTORADO")
  names(edu)[names(edu) == "NUMERO.ID.ORIENTADOR.DOUTORADO"] = "LATTES.ID.ORIENTADOR.DOUTORADO"

  g <- lcompID$NOME %in% edu$NOME.DOUTORADO
  if (any(!g[which(g == F)])) {
    lcompID_temp_doc <- lcompID[g, ]
    addbasic_temp_doc <- addbasic[g, ]
    address_temp_doc <- address[g, ]
    lcompID_temp <- lcompID[!g, ]
    addbasic_temp <- addbasic[!g, ]
    address_temp <- address[!g, ]
    lcompID <- cbind(lcompID_temp_doc, addbasic_temp_doc, address_temp_doc, edu[, -1])
    lcompID_temp <- cbind(lcompID_temp, addbasic_temp, address_temp)
    lcompID <- plyr::rbind.fill(lcompID, lcompID_temp)
  } else {
    lcompID <- cbind(lcompID, addbasic, address, edu[, -1])
  }

  lcompID <- as.data.frame(lcompID)
  names(lcompID) <- gsub("[-]", ".", names(lcompID))
  names(lcompID) <- gsub("[.]DE[.]|[.]DO[.]", ".", names(lcompID))

  lcompID <- lcompID %>% arrange(NOME)

  return(lcompID)

}
