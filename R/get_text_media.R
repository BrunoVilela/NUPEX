get_text_media <- function(xmls_list) {
  paper <- xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`TEXTOS-EM-JORNAIS-OU-REVISTAS`
  return(.general_func(paper, xmls_list))
}


