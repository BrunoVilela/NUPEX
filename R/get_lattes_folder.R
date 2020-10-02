#' Get all lattes information
#'
#' It summarizes multiple XML lattes data into a list of tables
#' (in tibble format) separated by the type of information.
#'
#' @param folder_path the path of the folder containing the XML files obtained from the lattes plataform
#'
#' @return A list of tibbles
#'
#' @examples
#' folder_path <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(folder_path)
#'
#' @export
#'

get_lattes_folder <- function(folder_path) {
  xmls <- list.files(folder_path, ".xml", full.names = TRUE)
  temp <- get_lattes(xmls[1])
  nome <- temp[[1]]$`NOME-COMPLETO`
  list_lattes <- sapply(temp, .add_names, y = nome)
  n_lattes <- length(xmls)
  if (n_lattes > 1) {
    for (i in 2:n_lattes) {
      temp <- get_lattes(xmls[i])
      nome <- temp[[1]]$`NOME-COMPLETO`
      temp2 <- sapply(temp, .add_names, y = nome)
      for (j in 1:length(temp2)) {
      temp3 <- plyr::rbind.fill(as.data.frame(list_lattes[[j]]),
                                             as.data.frame(temp2[[j]]))
      list_lattes[[j]] <- tibble::as_tibble(temp3)
      }
    }
  }
  return(list_lattes)
}

.add_names <- function(x, y){
  if(is.null(x)) {
    tibble::tibble(NULL)
  } else {
    tibble::add_column(x, "NOME" = y, .before = 1)
  }
}
