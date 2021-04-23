# Auxiliary function to extract a list of IDs

#' @author Domingos Cardoso
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @export
#'

listID <- function (lattesdata,
                    collabs = NULL) {

  # Loop over each cell to replace blank cells with NA
  lattesdata$basic[] <- lapply(lattesdata$basic, gsub, pattern = "^$", replacement = NA)

  IDs <- lattesdata[["basic"]]

  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(IDs)[1] != "NOME") {
    IDs <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                      IDs)
  }

  IDs <- IDs %>% select("NOME", "ORCID-ID", "NUMERO-IDENTIFICADOR", "DATA-ATUALIZACAO")

  names(IDs) <- gsub("[-]", ".", names(IDs))
  IDs$ORCID.ID <- gsub("^$", NA, IDs$ORCID.ID)

  IDs$NUMERO.IDENTIFICADOR <- paste0("http://lattes.cnpq.br/", IDs$NUMERO.IDENTIFICADOR)

  names(IDs)[names(IDs) == "NUMERO.IDENTIFICADOR"] = "LATTES"

  if (!is.null(collabs)) {
    IDs$VINCULO <- NA
    IDs$VINCULO[IDs$NOME %in% collabs] <- "Colaborador"
    IDs$VINCULO[is.na(IDs$VINCULO)] <- "Permanente"
  }

  return(IDs)

}
