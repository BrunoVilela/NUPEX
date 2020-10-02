get_basic_data <- function(xmls_list) {
  paper <- tibble::as_tibble(as.list(xmls_list$`DADOS-GERAIS`$.attrs))
  return(paper)
}


