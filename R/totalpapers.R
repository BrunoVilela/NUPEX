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
#' lattes_data <- get_lattes_folder(path_lattes)
#' lpapers <- listpapers(lattes_data,
#'                       capesdata = capes_qualis,
#'                       quadre = c(2017, 2020))
#' tpapers <- totalpapers(paperdata = lpapers,
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
#' tpapers <- totalpapers(paperdata = lpapers,
#'                        lattes_folder_data,
#'                        collabs = collaborators)
#'}
#'
#' @export
#'

totalpapers <- function(paperdata,
                        lattesdata,
                        collabs = NULL) {

  IDs <- listID(lattesdata,
                collabs = collabs)
  tpapers <- IDs

  tpapers$TOTAL.ARTIGOS <- NA
  tpapers$PRIMEIRO.AUTOR <- NA
  tpapers$ULTIMO.AUTOR <- NA
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

  if (any(!unique(IDs$NOME) %in% unique(paperdata$NOME))) {
    t <- unique(IDs$NOME) %in% unique(paperdata$NOME)
    for (i in seq_along(names(table(paperdata$NOME)))) {
      tpapers$TOTAL.ARTIGOS[t] <- ifelse(tpapers$NOME == names(table(paperdata$NOME))[i],
                                         as.vector(table(paperdata$NOME))[i],
                                         tpapers$TOTAL.ARTIGOS[t])
    }

    for (i in seq_along(unique(paperdata$NOME))) {
      # n <- 0
      # n <- n + i
      nm <- unique(paperdata$NOME)[i]
      temp <- data.frame()
      temp <- filter(paperdata, NOME == nm)

      tpapers$PRIMEIRO.AUTOR[t][tpapers$NOME == nm] <- as.vector(table(temp$PRIMEIRO.AUTOR)[2])
      tpapers$ULTIMO.AUTOR[t][tpapers$NOME == nm] <- as.vector(table(temp$ULTIMO.AUTOR)[2])
    }

    for (i in seq_along(unique(paperdata$NOME))) {
      nm <- unique(paperdata$NOME)[i]
      temp <- data.frame()
      temp <- filter(paperdata, NOME == nm)

      if (any(unique(temp$ESTRATO) == "NP", na.rm = TRUE)) {
        temp$ESTRATO[temp$ESTRATO == "NP"] <- NA
      }
      if (any(unique(temp$ESTRATO) == "A1", na.rm = TRUE)) {
        tpapers$TOTAL.A1[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A1"]
        temp$ESTRATO[temp$ESTRATO == "A1"] <- 87.5
      }
      if (any(unique(temp$ESTRATO) == "A2", na.rm = TRUE)) {
        tpapers$TOTAL.A2[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A2"]
        temp$ESTRATO[temp$ESTRATO == "A2"] <- 75
      }
      if (any(unique(temp$ESTRATO) == "A3", na.rm = TRUE)) {
        tpapers$TOTAL.A3[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A3"]
        temp$ESTRATO[temp$ESTRATO == "A3"] <- 62.5
      }
      if (any(unique(temp$ESTRATO) == "A4", na.rm = TRUE)) {
        tpapers$TOTAL.A4[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A4"]
        temp$ESTRATO[temp$ESTRATO == "A4"] <- 50
      }
      if (any(unique(temp$ESTRATO) == "B1", na.rm = TRUE)) {
        tpapers$TOTAL.B1[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B1"]
        temp$ESTRATO[temp$ESTRATO == "B1"] <- 37.5
      }
      if (any(unique(temp$ESTRATO) == "B2", na.rm = TRUE)) {
        tpapers$TOTAL.B2[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B2"]
        temp$ESTRATO[temp$ESTRATO == "B2"] <- 25
      }
      if(any(unique(temp$ESTRATO) == "B3", na.rm = TRUE)) {
        tpapers$TOTAL.B3[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B3"]
        temp$ESTRATO[temp$ESTRATO == "B3"] <- 12.5
      }
      if(any(unique(temp$ESTRATO) == "B4", na.rm = TRUE)) {
        tpapers$TOTAL.B4[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B4"]
        temp$ESTRATO[temp$ESTRATO == "B4"] <- 12.4
      }
      if (any(unique(temp$ESTRATO) == "C", na.rm = TRUE)) {
        tpapers$TOTAL.C[t][tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "C"]
        temp$ESTRATO[temp$ESTRATO == "C"] <- 0
      }


      tpapers$TOTAL.PERCENTIL[t][tpapers$NOME == nm] <- sum(as.numeric(temp$ESTRATO),na.rm = TRUE)
    }

  } else {

    # Contando numero de papers quando todos os autores possuem algum naquele quadrienio
    for (i in seq_along(names(table(paperdata$NOME)))) {
      tpapers$TOTAL.ARTIGOS <- ifelse(tpapers$NOME == names(table(paperdata$NOME))[i],
                                      as.vector(table(paperdata$NOME))[i],
                                      tpapers$TOTAL.ARTIGOS)
    }

    for (i in seq_along(unique(paperdata$NOME))) {
      nm <- unique(paperdata$NOME)[i]
      temp <- data.frame()
      temp <- filter(paperdata, NOME == nm)

      tpapers$PRIMEIRO.AUTOR[tpapers$NOME == nm] <- as.vector(table(temp$PRIMEIRO.AUTOR)[2])
      tpapers$ULTIMO.AUTOR[tpapers$NOME == nm] <- as.vector(table(temp$ULTIMO.AUTOR)[2])
    }

    for (i in seq_along(unique(paperdata$NOME))) {
      # n <- 0
      # n <- n + i
      nm <- unique(paperdata$NOME)[i]
      temp <- data.frame()
      temp <- filter(paperdata, NOME == nm)

      if (any(unique(temp$ESTRATO) == "NP", na.rm = TRUE)) {
        temp$ESTRATO[temp$ESTRATO == "NP"] <- NA
      }
      if (any(unique(temp$ESTRATO) == "A1", na.rm = TRUE)) {
        tpapers$TOTAL.A1[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A1"]
        temp$ESTRATO[temp$ESTRATO == "A1"] <- 87.5
      }
      if (any(unique(temp$ESTRATO) == "A2", na.rm = TRUE)) {
        tpapers$TOTAL.A2[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A2"]
        temp$ESTRATO[temp$ESTRATO == "A2"] <- 75
      }
      if (any(unique(temp$ESTRATO) == "A3", na.rm = TRUE)) {
        tpapers$TOTAL.A3[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A3"]
        temp$ESTRATO[temp$ESTRATO == "A3"] <- 62.5
      }
      if (any(unique(temp$ESTRATO) == "A4", na.rm = TRUE)) {
        tpapers$TOTAL.A4[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "A4"]
        temp$ESTRATO[temp$ESTRATO == "A4"] <- 50
      }
      if (any(unique(temp$ESTRATO) == "B1", na.rm = TRUE)) {
        tpapers$TOTAL.B1[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B1"]
        temp$ESTRATO[temp$ESTRATO == "B1"] <- 37.5
      }
      if (any(unique(temp$ESTRATO) == "B2", na.rm = TRUE)) {
        tpapers$TOTAL.B2[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B2"]
        temp$ESTRATO[temp$ESTRATO == "B2"] <- 25
      }
      if (any(unique(temp$ESTRATO) == "B3", na.rm = TRUE)) {
        tpapers$TOTAL.B3[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B3"]
        temp$ESTRATO[temp$ESTRATO == "B3"] <- 12.5
      }
      if (any(unique(temp$ESTRATO) == "B4", na.rm = TRUE)) {
        tpapers$TOTAL.B4[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "B4"]
        temp$ESTRATO[temp$ESTRATO == "B4"] <- 12.4
      }
      if (any(unique(temp$ESTRATO) == "C", na.rm = TRUE)) {
        tpapers$TOTAL.C[tpapers$NOME == nm] <- table(temp$ESTRATO)[names(table(temp$ESTRATO)) == "C"]
        temp$ESTRATO[temp$ESTRATO == "C"] <- 0
      }


      tpapers$TOTAL.PERCENTIL[tpapers$NOME == nm] <- sum(as.numeric(temp$ESTRATO),na.rm = TRUE)
    }
  }
  # mean(tpapers$TOTAL.PERCENTIL)
  # median(tpapers$TOTAL.PERCENTIL)

  return(tpapers)

}
