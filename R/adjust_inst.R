# Auxiliary function to adjust names of funding institutions and agencies

#' @author Domingos Cardoso
#'
#' @export
#'

adjust_inst <- function(lproj) {


  # Main Brazilian federal funding agencies
  braz_out <- c("CNPq|Conselho Nacional de Desenvolvimento Científico e Tecnológico",
                "CAPES|Coordenação de Aperfeiçoamento de Pessoal de Nível Superior",
                "FINEP|Financiadora de Estudos e Projetos",
                "Ministério da Ciência, Tecnologia",
                "Ministério do Meio Ambiente",
                "Ministério da Saúde",
                "Ministério da Educação")

  braz_in <- c("CNPq", "CAPES", "FINEP", "MCTI", "MMA", "MS", "MEC")

  for (i in seq_along(braz_out)) {

    n <- sapply(braz_out[i],
                grepl, lproj[, -c(1:11, length(lproj))])
    if (any(n)) {
      ag <- .fundagencies(braz_out[i],
                     lproj)
      lproj <- .replace_ag(braz_in[i], n, ag, lproj)
    }

  }


  # FAPs
  faps_out <- c("FAPEAM|Fundação de Amparo [aà] Pesquisa do Estado do Amazonas",
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
                "Fundação Araucária|Fundação Araucária de Apoio ao Desenvolvimento Científico e Tecnológico do Estado do Paraná")

  faps_in <- gsub("[|].*", "", faps_out)
  faps_in <- gsub(".*Espírito Santo", "FAPES", faps_in)
  faps_in <- gsub("Fundação Araucária.*", "FA", faps_in)

  for (i in seq_along(faps_out)) {

    n <- sapply(faps_out[i],
                grepl, lproj[, -c(1:11, length(lproj))])
    if (any(n)) {
      ag <- .fundagencies(faps_out[i],
                     lproj)
      lproj <- .replace_ag(faps_in[i], n, ag, lproj)
    }

  }


  # Brazilian universities
  univ_out <- c("UFBA|Universidade Federal da Bahia",
                "UEFS|Universidade Estadual de Feira de Santana",
                "UESC|Universidade Estadual de Santa Cruz",
                "UFMG|Universidade Federal de Minas Gerais",
                "USP|Universidade do Estado de São Paulo",
                "UFRJ|Universidade Federal do Rio de Janeiro")

  univ_in <- gsub("[|].*", "", univ_out)

  for (i in seq_along(univ_out)) {

    n <- sapply(univ_out[i],
                grepl, lproj[, -c(1:11, length(lproj))])
    if (any(n)) {
      ag <- .fundagencies(univ_out[i],
                     lproj)
      lproj <- .replace_ag(univ_in[i], n, ag, lproj)
    }

  }


  # Brazilian private funding agencies
  priv_out <- c("Petrobras|Petrobrás|Petróleo Brasileiro|CENPES|Centro de Pesquisa e Desenvolvimento Leopoldo Américo Miguêz de Mello",
                "Boticario|Fundação O Boticário de Proteção à Natureza",
                "Serrapilheira|Instituto Serrapilheira",
                "EMBRAPA|Empresa Brasileira de Pesquisa Agropecuária",
                "SENAI|Serviço Nacional de Aprendizagem Industrial",
                "Fiocruz|FIOCRUZ|Fundação Oswaldo Cruz",
                "Instituto Chico Mendes|Instituto Chico Mendes de Conservação da Biodiversidade",
                "Banco do Nordeste",
                "\\sVALE$|^Vale$|Companhia Vale do Rio Doce|Instituto Tecnológico Vale")

  priv_in <- c("Petrobras", "O Boticario", "Instituto Serrapilheira", "EMBRAPA",
               "SENAI", "Fiocruz", "ICMBio", "Banco do Nordeste", "VALE")

  for (i in seq_along(priv_out)) {

    n <- sapply(priv_out[i],
                grepl, lproj[, -c(1:11, length(lproj))])
    if (any(n)) {
      ag <- .fundagencies(priv_out[i],
                     lproj)
      lproj <- .replace_ag(priv_in[i], n, ag, lproj)
    }

  }


  # Foreign funding agencies
  inter_out <- c("NSF|National Science Foundation",
                "Royal Society",
                "NERC|Natural Environment Research Council",
                "Kew|Royal Botanic Gardens, Kew",
                "NIH|National Institutes [oO]f Health|National Institute [oO]f Health",
                "Novo Nordisk Foundation|NovoNordisk Foundation|Novo Nordisk Fonden",
                "European Commission",
                "OMS|Organiza[çcãa]o Mundial da Sa[uú]de|World Health Organization",
                "Food and Agricultural Organization of the United Nations|Organização das Nações Unidas para a Agricultura",
                "European Research Concil|European Research Council",
                "World Wildlife Fund")

  inter_in <- c("NSF", "Royal Society", "NERC", "Kew",
                "National Institutes of Health",
                "Novo Nordisk Foundation",
                "European Commission",
                "WHO", "FAO", "ERC", "WWF")

  for (i in seq_along(inter_out)) {

    n <- sapply(inter_out[i],
                grepl, lproj[, -c(1:11, length(lproj))])
    if (any(n)) {
      ag <- .fundagencies(inter_out[i],
                     lproj)
      lproj <- .replace_ag(inter_in[i], n, ag, lproj)
    }

  }

  return(lproj)

}


# New small auxiliary functions to standardize the names of Institutions and Agencies
.fundagencies <- function(x, y) {

  y <- y[, -c(1:11, length(y))]

  g <- sapply(x, grepl, y)

  res <- list()
  for (i in seq_along(names(y)[g])) {

    res[[i]] <- grepl(x, y[, names(y)[g][i]])
  }
  return(res)
}

.replace_ag <- function(x, n, ag, lproj) {
  for (i in 1:length(ag)) {
    if (length(ag) == 1) {
      lproj[, -c(1:11, length(lproj))][, n][ag[[i]]] <- x
    } else {
      lproj[, -c(1:11, length(lproj))][, n][i][ag[[i]], ] <- x
    }
  }
  return(lproj)
}
