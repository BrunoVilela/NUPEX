# Auxiliary function to adjust names of funding institutions and agencies

#' @author Domingos Cardoso
#'
#' @export
#'

adjust_inst <- function(lproj) {

  # Main Brazilian federal funding agencies
  n <- sapply("CNPq|Conselho Nacional de Desenvolvimento Científico e Tecnológico",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("CNPq|Conselho Nacional de Desenvolvimento Científico e Tecnológico",
                   lproj)
    lproj <- replace_ag("CNPq", n, ag, lproj)
  }

  n <- sapply("CAPES|Coordenação de Aperfeiçoamento de Pessoal de Nível Superior",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("CAPES|Coordenação de Aperfeiçoamento de Pessoal de Nível Superior",
                   lproj)
    lproj <- replace_ag("CAPES", n, ag, lproj)
  }

  n <- sapply("FINEP|Financiadora de Estudos e Projetos",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FINEP|Financiadora de Estudos e Projetos",
                   lproj)
    lproj <- replace_ag("FINEP", n, ag, lproj)
  }

  n <- sapply("Ministério da Ciência, Tecnologia",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Ministério da Ciência, Tecnologia",
                   lproj)
    lproj <- replace_ag("MCTI", n, ag, lproj)
  }

  n <- sapply("Ministério do Meio Ambiente",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Ministério do Meio Ambiente",
                   lproj)
    lproj <- replace_ag("MMA", n, ag, lproj)
  }

  n <- sapply("Ministério da Saúde",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Ministério da Saúde",
                   lproj)
    lproj <- replace_ag("MS", n, ag, lproj)
  }

  n <- sapply("Ministério da Educação",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Ministério da Educação",
                   lproj)
    lproj <- replace_ag("MEC", n, ag, lproj)
  }

  # FAPs
  # NORTE
  n <- sapply("FAPEAM|Fundação de Amparo [aà] Pesquisa do Estado do Amazonas",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEAM|Fundação de Amparo [aà] Pesquisa do Estado do Amazonas",
                   lproj)
    lproj <- replace_ag("FAPEAM", n, ag, lproj)
  }

  n <- sapply("FAPEAP|Fundação de Amparo [aà] Pesquisa do Amapá",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEAP|Fundação de Amparo [aà] Pesquisa do Amapá",
                   lproj)
    lproj <- replace_ag("FAPEAP", n, ag, lproj)
  }

  n <- sapply("FAPESPA|Fundação Amazônia Paraense de Amparo [aà] Pesquisa",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPESPA|Fundação Amazônia Paraense de Amparo [aà] Pesquisa",
                   lproj)
    lproj <- replace_ag("FAPESPA", n, ag, lproj)
  }

  n <- sapply("FAPERO|Fundação de Amparo [aà] Pesquisa do Estado de Rondônia",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPERO|Fundação de Amparo [aà] Pesquisa do Estado de Rondônia",
                   lproj)
    lproj <- replace_ag("FAPERO", n, ag, lproj)
  }

  n <- sapply("FAPERR|Fundação de Amparo [aà] Pesquisa do Estado de Roraima",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPERR|Fundação de Amparo [aà] Pesquisa do Estado de Roraima",
                   lproj)
    lproj <- replace_ag("FAPERR", n, ag, lproj)
  }

  # NORDESTE
  n <- sapply("FAPESB|Fundação de Amparo [aà] Pesquisa do Estado da Bahia",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPESB|Fundação de Amparo [aà] Pesquisa do Estado da Bahia",
                   lproj)
    lproj <- replace_ag("FAPESB", n, ag, lproj)
  }

  n <- sapply("FACEPE|Fundação de Amparo à Ciência e Tecnologia do Estado de Pernambuco",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FACEPE|Fundação de Amparo [aà] Ciência e Tecnologia do Estado de Pernambuco",
                   lproj)
    lproj <- replace_ag("FACEPE", n, ag, lproj)
  }

  n <- sapply("FAPEAL|Fundação de Amparo [aà] Pesquisa do Estado de Alagoas",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEAL|Fundação de Amparo [aà] Pesquisa do Estado de Alagoas",
                   lproj)
    lproj <- replace_ag("FAPEAL", n, ag, lproj)
  }

  n <- sapply("FAPEMA|Fundação de Amparo à Pesquisa ao Desenvolv. Científico e Tecnológico - MA|Fundação de Amparo à Pesquisa e ao Desenvolvimento Científico e Tecnológico do Maranhão",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEMA|Fundação de Amparo à Pesquisa ao Desenvolv. Científico e Tecnológico - MA|Fundação de Amparo à Pesquisa e ao Desenvolvimento Científico e Tecnológico do Maranhão",
                   lproj)
    lproj <- replace_ag("FAPEMA", n, ag, lproj)
  }

  n <- sapply("FAPESQ|Fundação de Apoio [aà] Pesquisa do Estado da Paraíba",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPESQ|Fundação de Apoio [aà] Pesquisa do Estado da Paraíba",
                   lproj)
    lproj <- replace_ag("FAPESQ", n, ag, lproj)
  }

  n <- sapply("FAPEPI|Fundação de Amparo [aà] Pesquisa do Estado do Piauí",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEPI|Fundação de Amparo [aà] Pesquisa do Estado do Piauí",
                   lproj)
    lproj <- replace_ag("FAPEPI", n, ag, lproj)
  }

  n <- sapply("FAPERN|Fundação de Apoio [aà] Pesquisa do Estado do Rio Grande do Norte",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPERN|Fundação de Apoio [aà] Pesquisa do Estado do Rio Grande do Norte",
                   lproj)
    lproj <- replace_ag("FAPERN", n, ag, lproj)
  }

  n <- sapply("FAPITEC|Fundação de Apoio [aà] Pesquisa e [aà] Inovação Tecnológica do Estado de Sergipe",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPITEC|Fundação de Apoio [aà] Pesquisa e [aà] Inovação Tecnológica do Estado de Sergipe",
                   lproj)
    lproj <- replace_ag("FAPITEC", n, ag, lproj)
  }

  # CENTRO-OESTE
  n <- sapply("FUNDECT|Fundação de Apoio e Desenvolvimento do Ensino, Ciência e Tecnologia do MS|Fundação de Apoio ao Desenvolvimento do Ensino, Ciência e Tecnologia do Estado de Mato Grosso do Sul",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FUNDECT|Fundação de Apoio e Desenvolvimento do Ensino, Ciência e Tecnologia do MS|Fundação de Apoio ao Desenvolvimento do Ensino, Ciência e Tecnologia do Estado de Mato Grosso do Sul",
                   lproj)
    lproj <- replace_ag("FUNDECT", n, ag, lproj)
  }

  n <- sapply("FAPEG|Fundação de Amparo [aà] Pesquisa do Estado de Goiás",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEG|Fundação de Amparo [aà] Pesquisa do Estado de Goiás",
                   lproj)
    lproj <- replace_ag("FAPEG", n, ag, lproj)
  }

  n <- sapply("FAPEMAT|Fundação de Amparo [aà] Pesquisa do Estado de Mato Grosso",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEMAT|Fundação de Amparo [aà] Pesquisa do Estado de Mato Grosso",
                   lproj)
    lproj <- replace_ag("FAPEMAT", n, ag, lproj)
  }

  n <- sapply("FAPT|Fundação de Amparo [aà] Pesquisa do Estado do Tocantins",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPT|Fundação de Amparo [aà] Pesquisa do Estado do Tocantins",
                   lproj)
    lproj <- replace_ag("FAPT", n, ag, lproj)
  }

  # SUDESTE
  n <- sapply("Fundação de Amparo [aà] Pesquisa do Espírito Santo",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Fundação de Amparo [aà] Pesquisa do Espírito Santo",
                   lproj)
    lproj <- replace_ag("FAPES", n, ag, lproj)
  }

  n <- sapply("FAPEMIG|fapemig|Fundação de Amparo [aà] Pesquisa do Estado de Minas Gerais",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPEMIG|fapemig|Fundação de Amparo [aà] Pesquisa do Estado de Minas Gerais",
                   lproj)
    lproj <- replace_ag("FAPEMIG", n, ag, lproj)
  }

  n <- sapply("FAPESP|Fundação de Amparo [aà] Pesquisa do Estado de São Paulo",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPESP|Fundação de Amparo [aà] Pesquisa do Estado de São Paulo",
                   lproj)
    lproj <- replace_ag("FAPESP", n, ag, lproj)
  }

  n <- sapply("FAPERJ|Fundação Carlos Chagas Filho de Amparo|Fundação de Amparo [aà] Pesquisa do Estado do Rio de Janeiro",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPERJ|Fundação Carlos Chagas Filho de Amparo|Fundação de Amparo [aà] Pesquisa do Estado do Rio de Janeiro",
                   lproj)
    lproj <- replace_ag("FAPERJ", n, ag, lproj)
  }

  # SUL
  n <- sapply("FAPERGS|Fundação de Amparo [aà] Pesquisa do Estado do Rio Grande do Sul",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPERGS|Fundação de Amparo [aà] Pesquisa do Estado do Rio Grande do Sul",
                   lproj)
    lproj <- replace_ag("FAPERGS", n, ag, lproj)
  }

  n <- sapply("FAPESC|Fundação de Amparo [aà] Pesquisa e Inovação de Santa Catarina",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("FAPESC|Fundação de Amparo [aà] Pesquisa e Inovação de Santa Catarina",
                   lproj)
    lproj <- replace_ag("FAPESC", n, ag, lproj)
  }

  n <- sapply("Fundação Araucária|Fundação Araucária de Apoio ao Desenvolvimento Científico e Tecnológico do Estado do Paraná",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Fundação Araucária|Fundação Araucária de Apoio ao Desenvolvimento Científico e Tecnológico do Estado do Paraná",
                   lproj)
    lproj <- replace_ag("FA", n, ag, lproj)
  }

  # Universities
  n <- sapply("UFBA|Universidade Federal da Bahia",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("UFBA|Universidade Federal da Bahia",
                   lproj)
    lproj <- replace_ag("UFBA", n, ag, lproj)
  }

  n <- sapply("UEFS|Universidade Estadual de Feira de Santana",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("UEFS|Universidade Estadual de Feira de Santana",
                   lproj)
    lproj <- replace_ag("UEFS", n, ag, lproj)
  }

  n <- sapply("UFMG|Universidade Federal de Minas Gerais",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("UFMG|Universidade Federal de Minas Gerais",
                   lproj)
    lproj <- replace_ag("UFMG", n, ag, lproj)
  }

  n <- sapply("UESC|Universidade Estadual de Santa Cruz",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("UESC|Universidade Estadual de Santa Cruz",
                   lproj)
    lproj <- replace_ag("UESC", n, ag, lproj)
  }

  n <- sapply("Petrobras|Petrobrás|Petróleo Brasileiro|CENPES|Centro de Pesquisa e Desenvolvimento Leopoldo Américo Miguêz de Mello",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Petrobras|Petrobrás|Petróleo Brasileiro|CENPES|Centro de Pesquisa e Desenvolvimento Leopoldo Américo Miguêz de Mello",
                   lproj)
    lproj <- replace_ag("Petrobras", n, ag, lproj)
  }

  n <- sapply("Boticario|Fundação O Boticário de Proteção à Natureza",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Boticario|Fundação O Boticário de Proteção à Natureza",
                   lproj)
    lproj <- replace_ag("O Boticario", n, ag, lproj)
  }

  n <- sapply("Serrapilheira|Instituto Serrapilheira",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Serrapilheira|Instituto Serrapilheira",
                   lproj)
    lproj <- replace_ag("Instituto Serrapilheira", n, ag, lproj)
  }

  n <- sapply("EMBRAPA|Empresa Brasileira de Pesquisa Agropecuária",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("EMBRAPA|Empresa Brasileira de Pesquisa Agropecuária",
                   lproj)
    lproj <- replace_ag("EMBRAPA", n, ag, lproj)
  }

  n <- sapply("SENAI|Serviço Nacional de Aprendizagem Industrial",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("SENAI|Serviço Nacional de Aprendizagem Industrial",
                   lproj)
    lproj <- replace_ag("SENAI", n, ag, lproj)
  }

  n <- sapply("Fiocruz|FIOCRUZ|Fundação Oswaldo Cruz",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Fiocruz|FIOCRUZ|Fundação Oswaldo Cruz",
                   lproj)
    lproj <- replace_ag("Fiocruz", n, ag, lproj)
  }

  n <- sapply("Instituto Chico Mendes|Instituto Chico Mendes de Conservação da Biodiversidade",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Instituto Chico Mendes|Instituto Chico Mendes de Conservação da Biodiversidade",
                   lproj)
    lproj <- replace_ag("ICMBio", n, ag, lproj)
  }

  n <- sapply("Banco do Nordeste",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Banco do Nordeste",
                   lproj)
    lproj <- replace_ag("Banco do Nordeste", n, ag, lproj)
  }

  n <- sapply("\\sVALE$|^Vale$|Companhia Vale do Rio Doce|Instituto Tecnológico Vale",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("\\sVALE$|^Vale$|Companhia Vale do Rio Doce|Instituto Tecnológico Vale",
                   lproj)
    lproj <- replace_ag("VALE", n, ag, lproj)
  }


  # Foreign funding agencies
  n <- sapply("NSF|National Science Foundation",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("NSF|National Science Foundation",
                   lproj)
    lproj <- replace_ag("NSF", n, ag, lproj)
  }

  n <- sapply("Royal Society",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Royal Society",
                   lproj)
    lproj <- replace_ag("Royal Society", n, ag, lproj)
  }

  n <- sapply("NERC|Natural Environment Research Council",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("NERC|Natural Environment Research Council",
                   lproj)
    lproj <- replace_ag("NERC", n, ag, lproj)
  }

  n <- sapply("Kew|Royal Botanic Gardens, Kew",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Kew|Royal Botanic Gardens, Kew",
                   lproj)
    lproj <- replace_ag("Kew", n, ag, lproj)
  }

  n <- sapply("NIH|National Institutes [oO]f Health|National Institute [oO]f Health",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("NIH|National Institutes [oO]f Health|National Institute [oO]f Health",
                   lproj)
    lproj <- replace_ag("National Institutes of Health", n, ag, lproj)
  }

  n <- sapply("Novo Nordisk Foundation|NovoNordisk Foundation|Novo Nordisk Fonden",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Novo Nordisk Foundation|NovoNordisk Foundation|Novo Nordisk Fonden",
                   lproj)
    lproj <- replace_ag("Novo Nordisk Foundation", n, ag, lproj)
  }

  n <- sapply("European Commission",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("European Commission",
                   lproj)
    lproj <- replace_ag("European Commission", n, ag, lproj)
  }

  n <- sapply("OMS|Organiza[çcãa]o Mundial da Sa[uú]de|World Health Organization",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("OMS|Organiza[çcãa]o Mundial da Sa[uú]de|World Health Organization",
                   lproj)
    lproj <- replace_ag("WHO", n, ag, lproj)
  }

  n <- sapply("Food and Agricultural Organization of the United Nations|Organização das Nações Unidas para a Agricultura",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("Food and Agricultural Organization of the United Nations|Organização das Nações Unidas para a Agricultura",
                   lproj)
    lproj <- replace_ag("FAO", n, ag, lproj)
  }

  n <- sapply("European Commission",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("European Commission",
                   lproj)
    lproj <- replace_ag("European Commission", n, ag, lproj)
  }

  n <- sapply("European Research Concil|European Research Council",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("European Research Concil|European Research Council",
                   lproj)
    lproj <- replace_ag("ERC", n, ag, lproj)
  }

  n <- sapply("World Wildlife Fund",
              grepl, lproj[, -c(1:11, length(lproj))])
  if (any(n)) {
    ag <- agencies("World Wildlife Fund",
                   lproj)
    lproj <- replace_ag("WWF", n, ag, lproj)
  }

  return(lproj)

}


# New small auxiliary functions to standarde the names of Institutions and Agencies
agencies <- function(x, y) {

  y <- y[, -c(1:11, length(y))]

  g <- sapply(x, grepl, y)

  res <- list()
  for (i in seq_along(names(y)[g])) {

    res[[i]] <- grepl(x, y[, names(y)[g][i]])
  }
  return(res)
}

replace_ag <- function(x, n, ag, lproj) {
  for (i in 1:length(ag)) {
    if (length(ag) == 1) {
      lproj[, -c(1:11, length(lproj))][, n][ag[[i]]] <- x
    } else {
      lproj[, -c(1:11, length(lproj))][, n][i][ag[[i]], ] <- x
    }
  }
  return(lproj)
}
