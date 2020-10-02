get_other_publications <- function(xmls_list) {
  paper <- xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA`
  return(.general_func(paper, xmls_list))
}
