get_conference_pubs <- function(xmls_list) {
  paper <-   xmls_list$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`
  return(.general_func(paper, xmls_list))
}


    