#' Produce total paper production
#'
#' @author Domingos Cardoso
#'
#' @description It uses the list of publication (data frame format) derived by
#' \code{\link{listpapers}} so as to produce a summary of total paper production.
#' If the capesdata file was provided when retrieving the complete list of papers,
#' then this function will also return the total paper production in each stratum
#' of CAPES journal classification, as well as the total percentil
#'
#' @param paperdata table (data frame format) derived by \code{\link{listpapers}}
#' function, which has to be used so as to get the total number of published papers
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}
#'
#' @param collabs vector containing the complete names of collaborators for the
#' Graduate Program; the provided names have to be exactly as shown in their
#' Lattes profiles; the non-listed collaborators will automatically have their
#' "VINCULO" defined as "Permanente"
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes(path_lattes)
#' lpapers <- listpapers(lattes_data,
#'                       capesdata = capes_qualis,
#'                       quadre = c(2017, 2020))
#' tpapers <- totalpapers(lpapers,
#'                        lattes_data)
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' data(capes_qualis)
#' lpapers <- listpapers(lattes_folder_data,
#'                       capesdata = capes_qualis,
#'                       quadre = c(2017, 2020))
#' collaborators <- c("Suzana Telles da Cunha Lima",
#'                    "Cláudia Dias de Santana")
#' tpapers <- totalpapers(lpapers,
#'                        lattes_folder_data,
#'                        collabs = collaborators)
#'}
#'
#' @export
#'

totalpapers <- function(paperdata,
                        lattesdata,
                        collabs = NULL) {

  # Filter multiple values on a string column in dplyr
  #IDs <- IDs %>% filter(NOME %in% sort(unique(IDs$NOME))[t])
  IDs <- listID(lattesdata,
                collabs = collabs)
  tpapers <- IDs

  tpapers$TOTAL.ARTIGOS <- NA
  tpapers$PRIMEIRO.AUTOR <- NA
  tpapers$ULTIMO.AUTOR <- NA

  capestrata <- names(paperdata) == "ESTRATO"
  if (any(capestrata)) {
    tpapers$TOTAL.A1 <- NA
    tpapers$TOTAL.A2 <- NA
    tpapers$TOTAL.A3 <- NA
    tpapers$TOTAL.A4 <- NA
    tpapers$TOTAL.B1 <- NA
    tpapers$TOTAL.B2 <- NA
    tpapers$TOTAL.B3 <- NA
    tpapers$TOTAL.B4 <- NA
    tpapers$TOTAL.C <- NA
    tpapers$TOTAL.PERCENTIL <- NA
  }

  for (i in seq_along(names(table(paperdata$NOME)))) {
    tpapers$TOTAL.ARTIGOS <- ifelse(tpapers$NOME == names(table(paperdata$NOME))[i],
                                    as.vector(table(paperdata$NOME))[i],
                                    tpapers$TOTAL.ARTIGOS)
  }

  for (i in seq_along(unique(paperdata$NOME))) {
    # n <- 0
    # n <- n + i
    nm <- unique(paperdata$NOME)[i]
    temp <- data.frame()
    temp <- filter(paperdata, NOME == nm)

    tpapers$PRIMEIRO.AUTOR[tpapers$NOME == nm] <- as.vector(table(temp$PRIMEIRO.AUTOR)[2])
    tpapers$ULTIMO.AUTOR[tpapers$NOME == nm] <- as.vector(table(temp$ULTIMO.AUTOR)[2])
  }

  if (any(capestrata)) {

    strata <- c("NP",
                "A1", "A2", "A3", "A4",
                "B1", "B2", "B3", "B4",
                "C")
    percent <- c(0,
                 87.5, 75, 62.5, 50,
                 37.5, 25, 12.5, 12.4,
                 0)
    for (i in seq_along(unique(paperdata$NOME))) {
      nm <- unique(paperdata$NOME)[i]
      temp <- data.frame()
      temp <- filter(paperdata, NOME == nm)
      for (n in seq_along(strata)) {
        if (any(unique(temp$ESTRATO) == strata[n], na.rm = TRUE)) {
          if (strata[n] == "NP") {
            temp$ESTRATO[temp$ESTRATO == strata[n]] <- percent[n]
          } else {
            tpapers[[eval(paste0("TOTAL.", strata[n]))]][tpapers$NOME == nm] <-
              table(temp$ESTRATO)[names(table(temp$ESTRATO)) == strata[n]]
            temp$ESTRATO[temp$ESTRATO == strata[n]] <- percent[n]
          }
        }
      }

      tpapers$TOTAL.PERCENTIL[tpapers$NOME == nm] <- sum(as.numeric(temp$ESTRATO), na.rm = TRUE)
    }

  }

  # mean(tpapers$TOTAL.PERCENTIL)
  # median(tpapers$TOTAL.PERCENTIL)

  return(tpapers)

}
