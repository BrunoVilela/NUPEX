#' Get all lattes information
#'
#' It summarizes multiple XML lattes data into a list of tables
#' (in tibble format) separated by the type of information.
#'
#' @param folder_path the path of the folder containing the XML files obtained from the lattes platform
#' @param progress if TRUE, progress bar is shown
#'
#' @return A list of tibbles
#'
#' @examples
#' folder_path <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(folder_path, FALSE)
#'
#' @export
#'

get_lattes_folder <- function(folder_path, progress = TRUE) {
  xmls <- list.files(folder_path, ".xml", full.names = TRUE)
  n_lattes <- length(xmls)
  if (progress) {
    pb <- utils::txtProgressBar(min = 1,
                                max = n_lattes,
                                style = 3)
  }
  temp <- get_lattes(xmls[1])
  nome <- temp[[1]]$`NOME-COMPLETO`
  list_lattes <- sapply(temp, .add_names, y = nome)
  if (progress) {
    utils::setTxtProgressBar(pb, 1)
  }
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
      if (progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
  }

  # Standartizing titles of published studies and names of scientific journals
  list_lattes <- std_titles(list_lattes)
  list_lattes <- adjust_journ(list_lattes)


  return(list_lattes)
}

.add_names <- function(x, y){
  if(is.null(x)) {
    tibble::tibble(NULL)
  } else {
    tibble::add_column(x, "NOME" = y, .before = 1)
  }
}
