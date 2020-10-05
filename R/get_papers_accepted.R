get_papers_accepted <- function(xmls_list) {
  paper <- xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-ACEITOS-PARA-PUBLICACAO`
  return(.general_func(paper, xmls_list))
}
