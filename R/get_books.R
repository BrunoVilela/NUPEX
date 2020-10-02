get_books <- function(xmls_list) {
  paper <- xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`LIVROS-PUBLICADOS-OU-ORGANIZADOS`
  return(.general_func(paper, xmls_list))
}

