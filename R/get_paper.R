get_papers <- function(xmls_list) {
  paper <- xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`
  return(.general_func(paper, xmls_list))
}

