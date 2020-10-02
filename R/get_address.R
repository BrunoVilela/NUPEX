get_address <- function(xmls_list) {
  paper <- xmls_list$`DADOS-GERAIS`$ENDERECO
  pre0 <- as.list(unlist(paper))
  names(pre0) <- gsub(".attrs.", "", names(pre0))
  pre <- data.frame(pre0)
  return(tibble::as_tibble(pre))
}


