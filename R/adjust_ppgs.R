# Auxiliar function for manual adjustments of ppgs codes
# it is to be used with an internal output of the functions ppgsuperv and listdefense

#' @author Domingos Cardoso
#'
#' @export
#'

adjust_ppgs <- function(x) {

  ufba <- c("Universidade Federal da Bahia",
            "Instituto de Biologia UFBA",
            "Instituto de Biologia da UFBA",
            "UFBA")
  if (any(x$NOME.INSTITUICAO %in% ufba)) {

    # Ajusting the name of the PPG Biodiversidade e Evolução - UFBA
    jj <- c("90000027", "90000028", "90000026", "24000183", "60052058", "90000023",
            "90000015", "90000010", "90000022", "90000013", "90000037", "90000093",
            "90000019", "90000038", "90000017", "90000033", "90000006", "90000056",
            "90000050", "90000024")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Federal da Bahia"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Biodiversidade e Evolução"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")
  }


  uefs <- c("Universidade Estadual de Feira de Santana")
  if (any(x$NOME.INSTITUICAO %in% uefs)) {

    # Ajusting the name of the PPG Botânica - UEFS
    jj <- c("90000007", "60040165", "60010746", "90000005", "90000004")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Estadual de Feira de Santana"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Botânica"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")

    # Ajusting the name of the PPG Recursos Genéticos Vegetais - UEFS
    jj <- c("60046945", "90000024")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Estadual de Feira de Santana"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Recursos Genéticos Vegetais"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")

    # Ajusting the name of the PPG Ecologia e Evolução - UEFS
    jj <- c("90000021")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Estadual de Feira de Santana"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Ecologia e Evolução"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")
  }


  uneb <- c("Universidade do Estado da Bahia")
  if (any(x$NOME.INSTITUICAO %in% uneb)) {

    # Ajusting the name of the PPG Biodiversidade Vegetal - UNEB
    jj <- c("60057408")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade do Estado da Bahia"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Biodiversidade Vegetal"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")
  }


  ufmg <- c("Universidade Federal de Minas Gerais")
  if (any(x$NOME.INSTITUICAO %in% ufmg)) {

    # Ajusting the name of the PPG Biodiversidade Vegetal - UFMG
    jj <- c("90000003", "90000002", "32000065", "90000001", "90000026")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Federal de Minas Gerais"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Bioquímica e Imunologia"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")

    # Ajusting the name of the PPG Bioinformática - UFMG
    jj <- c("60021403", "90000012", "90000039")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Federal de Minas Gerais"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Bioinformática"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")

    # Ajusting the name of the PPG Genética - UFMG
    jj <- c("60051108", "60001976", "60051116", "60051140")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Federal de Minas Gerais"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Genética"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")

    # Ajusting the name of the PPG Microbiologia - UFMG
    jj <- c("90000025", "60048441", "60048484")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Federal de Minas Gerais"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Microbiologia"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")

    # Ajusting the name of the PPG Nutrição e Saúde - UFMG
    jj <- c("90000015", "60772875", "90000023")
    x$NOME.INSTITUICAO[x$CODIGO.CURSO %in% jj] <- "Universidade Federal de Minas Gerais"
    x$NOME.CURSO[x$CODIGO.CURSO %in% jj] <- "Nutrição e Saúde"
    x$CODIGO.CURSO[x$CODIGO.CURSO %in% jj] <- paste0(sort(jj), collapse = ",")
  }

  return(x)

}
