get_books_chapters <- function(xmls_list) {
  paper <- xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`LIVROS-E-CAPITULOS`$`CAPITULOS-DE-LIVROS-PUBLICADOS`
  return(.general_func(paper, xmls_list))
}


