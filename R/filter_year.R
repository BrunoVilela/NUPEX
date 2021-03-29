#' Filter the year of lattes information
#'
#' It filters the data obtained from get_lattes_folder to a user specified year
#'
#' @param lattes_data output result from get_lattes_folder function.
#' @param year the year to filter the data for.
#'
#' @details The function will not filter education, basic information,
#' idioms, and fields of research(areas).
#' @return list of tibbles filtered by the specified year
#'
#' @examples
#' folder_path <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(folder_path)
#' lattes_filtered <- filter_year(lattes_folder_data, year = 2020)
#' summary_lattes <- summary_all(lattes_filtered)
#' @export

filter_year <- function(lattes_data, year) {
  posis <- list()
  for (i in 1:length(lattes_data)) {
    temp_names <- gsub("\\.", " ", colnames(lattes_data[[i]]))
    posis[[i]] <- grep("ANO", temp_names, fixed = TRUE)
  }
  names(posis) <- names(lattes_data)
  posis$education <- integer()
  posis$courses <- posis$courses[2]
  posis$conference <- posis$conference[1]
  # Have to be less then
  posis$journal_referee <- posis$journal_referee[1:2]
  posis$professional_activity <- posis$professional_activity[1:2]
  # otherpubs
  anos <- list()
  for (i in 1:length(lattes_data)) {
    anos[[i]] <- lattes_data[[i]][, posis[[i]]]
  }
  names(anos) <- names(lattes_data)

  if (!is.null(anos$other_pubs)) {
    year_others <- anos$other_pubs
    if (ncol(year_others) == 2) {
      anos$other_pubs <- tibble::as_tibble(as.numeric(ifelse(is.na(year_others$ANO),
                                                             year_others$`DADOS-BASICOS-DA-TRADUCAO.ANO`,
                                                             year_others$ANO)))
    }
  }
  if (!is.null(anos$supervision_ongoing)) {
    anos$supervision_ongoing <-
      tibble::add_column(anos$supervision_ongoing,
                       ANO.FIM = NA)
  }

  resultado <- lattes_data
  for (i in 1:length(lattes_data)) {
    resultado[[i]] <- aux_filter_year(y = anos[[i]],
                                      x = lattes_data[[i]],
                                      year = year)
  }
  names(resultado) <- names(anos)
  return(resultado)
}



aux_filter_year <- function(x, y, year) {
  if (tibble::is_tibble(y) & ncol(y) > 0) {
    if (ncol(y) == 2) {
      year_num <- y[, 2][[1]]
      y[, 2][[1]] <- ifelse(is.na(year_num) | year_num == "",
                    as.numeric(base::substr(base::Sys.Date(), 1, 4)),
                    year_num)
      year_num <- y[, 1][[1]]
      y[, 1][[1]] <- ifelse(is.na(year_num) | year_num == "",
                            as.numeric(base::substr(base::Sys.Date(), 1, 4)),
                            year_num)

      years_seq <- apply(y, 1, function(x)x[1]:x[2])
      pos <- sapply(years_seq, function(z, k)any(k%in%z), k = year)
    } else {
      pos <- as.numeric(y[[1]]) %in% year
    }
    x <- x[pos, ]
    return(x)
  } else {
    return(x)
  }
}
