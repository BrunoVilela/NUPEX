#' Get papers altmetric data
#'
#' It obtains altmetric paper data from a tibble obtained using
#' get_lattes or get_lattes_folder.
#'
#' @param papers table (tibble format) depicting papers published or accepted as obtained
#' from get_lattes or get_lattes_folder
#'
#' @return A table in tibble format.
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                      "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' alt <- get_altmetric(lattes_data$papers)
#'}
#' @export

get_altmetric <- function(papers) {
  n_alt <- nrow(papers)
  result <- list()
  for (i in 1:n_alt) {
    alt_1 <-
      try(rAltmetric::altmetrics(doi = papers$DOI[i]), silent = TRUE)
    if (class(alt_1) == "try-error") {
      result[[i]] <- tibble::tibble(doi = NA)
    } else {
      rem <- grep("history.", names(alt_1))
      alt_1 <- alt_1[-rem]
      result[[i]] <- tibble::as_tibble(lapply(alt_1, .collapse_names))
    }
  }
  result <- do.call(plyr::rbind.fill, result)
  return(tibble::as_tibble(result))
}

.collapse_names <- function(x) {
  ifelse(length(x) > 1, paste(x, collapse = "; "), x)
}
