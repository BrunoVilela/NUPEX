# Function to adjust names of funding agencies

# I still need to merge this one with the more general auxiliary function adjust_inst

#' @author Domingos Cardoso
#'
#' @export
#'

adjust_ag <- function(x) {

  l <- length(which(names(x) == "NOME.INSTITUICAO" | names(x) == "NOME.AGENCIA"))

  if (l == 1) {
    names(x)[names(x) == "NOME.INSTITUICAO"] <- "NOME.AGENCIA"
  }

  x$NOME.AGENCIA <- gsub("^$", NA, x$NOME.AGENCIA)

  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Coordenação de Aperfeiçoamento de Pessoal de Nível Superior"] <- "CAPES"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Conselho Nacional de Desenvolvimento Científico e Tecnológico"] <- "CNPq"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "(CNPq) Conselho Nacional de Desenvolvimento Científico e Tecnológico"] <- "CNPq"

  # NORTE
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado do Amazonas"] <- "FAPEAM"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Amapá"] <- "FAPEAP"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação Amazônia Paraense de Amparo à Pesquisa"] <- "FAPESPA"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de Rondônia"] <- "FAPERO"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de Roraima"] <- "FAPERR"

  # NORDESTE
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de Alagoas"] <- "FAPEAL"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado da Bahia"] <- "FAPESB"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa ao Desenvolv. Científico e Tecnológico - MA"] <- "FAPEMA"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa e ao Desenvolvimento Científico e Tecnológico do Maranhão"] <- "FAPEMA"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Apoio à Pesquisa do Estado da Paraíba"] <- "FAPESQ"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Ciência e Tecnologia do Estado de Pernambuco"] <- "FACEPE"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado do Piauí"] <- "FAPEPI"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Apoio à Pesquisa do Estado do Rio Grande do Norte"] <- "FAPERN"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Apoio à Pesquisa e a Inovação Tecnológica do Estado de Sergipe"] <- "FAPITEC"

  # SUL
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado do Rio Grande do Sul"] <- "FAPERGS"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa e Inovação de Santa Catarina"] <- "FAPESC"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação Araucária de Apoio ao Desenvolvimento Científico e Tecnológico do Estado do Paraná"] <- "FA"

  # SUDESTE
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Espírito Santo"] <- "FAPES"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de Minas Gerais"] <- "FAPEMIG"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "fapemig"] <- "FAPEMIG"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de São Paulo"] <- "FAPESP"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado do Rio de Janeiro"] <- "FAPERJ"

  # CENTRO-OESTE
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de Goiás"] <- "FAPEG"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado de Mato Grosso"] <- "FAPEMAT"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Apoio ao Desenvolvimento do Ensino, Ciência e Tecnologia do Estado de Mato Grosso do Sul"] <- "FUNDECT"
  x$NOME.AGENCIA[x$NOME.AGENCIA ==
                      "Fundação de Amparo à Pesquisa do Estado do Tocantins"] <- "FAPT"

  return(x)

}
