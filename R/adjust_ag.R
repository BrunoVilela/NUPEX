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

  agenc_out <- c("FAPEAM|Fundação de Amparo [aà] Pesquisa do Estado do Amazonas",
                 "FAPEAP|Fundação de Amparo [aà] Pesquisa do Amapá",
                 "FAPESPA|Fundação Amazônia Paraense de Amparo [aà] Pesquisa",
                 "FAPERO|Fundação de Amparo [aà] Pesquisa do Estado de Rondônia",
                 "FAPERR|Fundação de Amparo [aà] Pesquisa do Estado de Roraima",
                 "FAPESB|Fundação de Amparo [aà] Pesquisa do Estado da Bahia",
                 "FACEPE|Fundação de Amparo [aà] Ci[eê]ncia e Tecnologia do Estado de Pernambuco",
                 "FAPEAL|Fundação de Amparo [aà] Pesquisa do Estado de Alagoas",
                 "FUNCAP|Fundação Cearense de Apoio ao Desenvolvimento Científico e Tecnológico",
                 "FAPEMA|Fundação de Amparo [aà] Pesquisa ao Desenvolv. Científico e Tecnológico - MA|Fundação de Amparo [aà] Pesquisa e ao Desenvolvimento Científico e Tecnológico do Maranhão",
                 "FAPESQ|Fundação de Apoio [aà] Pesquisa do Estado da Paraíba",
                 "FAPEPI|Fundação de Amparo [aà] Pesquisa do Estado do Piauí",
                 "FAPERN|Fundação de Apoio [aà] Pesquisa do Estado do Rio Grande do Norte",
                 "FAPITEC|Fundação de Apoio [aà] Pesquisa e [aà] Inovação Tecnológica do Estado de Sergipe",
                 "FUNDECT|Fundação de Apoio e Desenvolvimento do Ensino, Ciência e Tecnologia do MS|Fundação de Apoio ao Desenvolvimento do Ensino, Ciência e Tecnologia do Estado de Mato Grosso do Sul",
                 "FAPEG|Fundação de Amparo [aà] Pesquisa do Estado de Goiás",
                 "FAPEMAT|Fundação de Amparo [aà] Pesquisa do Estado de Mato Grosso",
                 "FAPT|Fundação de Amparo [aà] Pesquisa do Estado do Tocantins",
                 "Fundação de Amparo [aà] Pesquisa do Espírito Santo",
                 "FAPEMIG|fapemig|Fundação de Amparo [aà] Pesquisa do Estado de Minas Gerais",
                 "FAPESP|Fundação de Amparo [aà] Pesquisa do Estado de São Paulo",
                 "FAPERJ|Fundação Carlos Chagas Filho de Amparo|Fundação de Amparo [aà] Pesquisa do Estado do Rio de Janeiro",
                 "FAPERGS|Fundação de Amparo [aà] Pesquisa do Estado do Rio Grande do Sul",
                 "FAPESC|Fundação de Amparo [aà] Pesquisa e Inovação de Santa Catarina",
                 "Fundação Araucária|Fundação Araucária de Apoio ao Desenvolvimento Científico e Tecnológico do Estado do Paraná",
                 "CAPES|Coordenação de Aperfeiçoamento de Pessoal de Nível Superior",
                 "CNPq|Conselho Nacional de Desenvolvimento Científico e Tecnológico",
                 "Fiocruz|FIOCRUZ",
                 "UFBA|Universidade Federal da Bahia",
                 "UEFS|Universidade Estadual de Feira de Santana",
                 "UESC|Universidade Estadual de Santa Cruz",
                 "UFMG|Ufmg|Universidade Federal de Minas Gerais",
                 "USP|Universidade do Estado de São Paulo",
                 "UFRJ|Universidade Federal do Rio de Janeiro",
                 "UFPE|Universidade Federal de Pernambuco")

  agenc_in <- gsub("[|].*", "", agenc_out)
  agenc_in <- gsub(".*Espírito Santo", "FAPES", agenc_in)
  agenc_in <- gsub("Fundação Araucária.*", "FA", agenc_in)

  for (i in seq_along(agenc_out)) {

    g <- grepl(agenc_out[i], x$NOME.AGENCIA)

    if (any(g)) {
      x$NOME.AGENCIA[g] <- agenc_in[i]
    }

  }

  return(x)

}
