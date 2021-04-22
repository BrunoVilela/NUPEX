#' Produce list of contributions in editorial board or as referees for scientific journals
#'
#' @author Domingos Cardoso
#'
#' @description It produces a data frame listing the scientific journals that have
#' contributing editorial board or referees, from a tibble obtained using
#' \code{\link{get_lattes}} or \code{\link{get_lattes_folder}}
#'
#' @param lattesdata table (tibble format) derived by \code{\link{get_lattes}} or
#' \code{\link{get_lattes_folder}}, that will be used to extract a list of
#' contributing editorial board or referees
#'
#' @param paperdata table (data frame format) derived by \code{\link{listpapers}}
#' function, which if provided may add the journal classification according to
#' CAPES strata, depending on how the list of paper production was generated
#'
#' @param quadre vector defining a time frame of years to extract the list of
#' contributing editorial board or referees
#'
#' @return Table in data frame format
#'
#' @examples
#' \dontrun{
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'                       "/lattes2.xml")
#' lattes_data <- get_lattes_folder(path_lattes)
#' data(capes_qualis)
#' lpapers <- listpapers(lattes_data,
#'                       capesdata = capes_qualis,
#'                       quadre = c(2017, 2020))
#' lpats <- listrev(lattes_data,
#'                  paperdata = lpapers
#'                  quadre = c(2017, 2020))
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' data(capes_qualis)
#' lpapers <- listpapers(lattes_folder_data,
#'                       capesdata = capes_qualis,
#'                       quadre = c(2017, 2020))
#' lrev <- listrev(lattes_folder_data,
#'                  paperdata = lpapers,
#'                  quadre = c(2017, 2020))
#'}
#'
#' @importFrom dplyr arrange filter select
#' @importFrom magrittr "%>%"
#'
#' @export
#'

listrev <- function(lattesdata,
                    paperdata = NULL,
                    quadre = NULL) {

  l <- length(lattesdata[["journal_editor"]]) + length(lattesdata[["journal_referee"]])
  if (l == 0) {
    stop("There are no listed contributions in editorial board or as referee
         for scientific journals in any of the parsed Lattes xml files")
  }

  # Loop over each cell to replace blank cells with NA
  lattesdata$journal_editor[] <- lapply(lattesdata$journal_editor, gsub, pattern = "^$", replacement = NA)

  jeditor <- lattesdata[["journal_editor"]]
  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(jeditor)[1] != "NOME") {
    jeditor <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                          jeditor)
  }

  jeditor <- jeditor %>%
    select("NOME", "OUTRO.VINCULO.INFORMADO", "NOME.INSTITUICAO", "ANO.INICIO", "ANO.FIM")
  colnames(jeditor)[colnames(jeditor) == "NOME.INSTITUICAO"] <- "TITULO.PERIODICO.OU.REVISTA"

  # Some cleaning of journal titles
  #jeditor <- adjust_journ(jeditor)

  temp <- jeditor %>% filter(NOME == jeditor$NOME[1])
  temp <- temp[!duplicated(temp$TITULO.PERIODICO.OU.REVISTA), ]
  for (i in 2:length(unique(jeditor$NOME))) {

    t <- jeditor %>% filter(NOME == unique(jeditor$NOME)[i])
    t <- t[!duplicated(t$TITULO.PERIODICO.OU.REVISTA), ]
    temp <- rbind(temp, t)

  }
  jeditor <- temp

  # Loop over each cell to replace blank cells with NA
  lattesdata$journal_referee[] <- lapply(lattesdata$journal_referee, gsub, pattern = "^$", replacement = NA)

  jreferee <- lattesdata[["journal_referee"]]
  # Adding the missing column NOME, when only one Lattes xml file is parsed by get_lattes
  if (names(jreferee)[1] != "NOME") {
    jreferee <- data.frame(NOME = lattesdata$basic$`NOME-COMPLETO`,
                           jreferee)
  }

  jreferee <- jreferee %>%
    select("NOME", "OUTRO.VINCULO.INFORMADO", "NOME.INSTITUICAO", "ANO.INICIO", "ANO.FIM")
  colnames(jreferee)[colnames(jreferee) == "NOME.INSTITUICAO"] <- "TITULO.PERIODICO.OU.REVISTA"

  # Some cleaning of journal titles
  #jreferee <- adjust_journ(jreferee)

  temp <- jreferee %>% filter(NOME == jreferee$NOME[1])
  temp <- temp[!duplicated(temp$TITULO.PERIODICO.OU.REVISTA), ]
  for (i in 2:length(unique(jreferee$NOME))) {

    t <- jreferee %>% filter(NOME == unique(jreferee$NOME)[i])
    t <- t[!duplicated(t$TITULO.PERIODICO.OU.REVISTA), ]
    temp <- rbind(temp, t)

  }
  jreferee <- temp


  lrev <- rbind(jeditor, jreferee)
  # Remove rows from data frame where ALL values are NA
  lrev <- lrev[rowSums(is.na(lrev[, 2:5])) != ncol(lrev[, 2:5]), ]

  colnames(lrev)[colnames(lrev) == "OUTRO.VINCULO.INFORMADO"] <- "VINCULO"

  lrev <- lrev %>% arrange(NOME, VINCULO, desc(ANO.INICIO))
  lrev$ANO.FIM <- gsub("^$", NA, lrev$ANO.FIM)

  # Adding strata for each journal
  if (!is.null(paperdata)) {
    lrev$ESTRATO <- NA
    lrev$ISSN <- NA

    paperdata <- paperdata %>% filter(!is.na(ESTRATO)) %>% select("TITULO.PERIODICO.OU.REVISTA",
                                                                  "ESTRATO", "ISSN")
    paperdata <- paperdata[!duplicated(paperdata$TITULO.PERIODICO.OU.REVISTA), ]

    t <- paperdata$TITULO.PERIODICO.OU.REVISTA %in% lrev$TITULO.PERIODICO.OU.REVISTA
    tj <- paperdata$TITULO.PERIODICO.OU.REVISTA[t]
    g <- lrev$TITULO.PERIODICO.OU.REVISTA %in% tj

    for (i in seq_along(lrev$TITULO.PERIODICO.OU.REVISTA[g])) {

      estr <- paperdata$ESTRATO[paperdata$TITULO.PERIODICO.OU.REVISTA %in% lrev$TITULO.PERIODICO.OU.REVISTA[g][i]]
      issn <- paperdata$ISSN[paperdata$TITULO.PERIODICO.OU.REVISTA %in% lrev$TITULO.PERIODICO.OU.REVISTA[g][i]]

      lrev$ESTRATO[g][i] <- estr
      lrev$ISSN[g][i] <- issn

    }

    lrev <- lrev %>% select("NOME", "VINCULO", "TITULO.PERIODICO.OU.REVISTA",
                            "ESTRATO", "ISSN", "ANO.INICIO", "ANO.FIM")
  }

  # Defining a base year
  if (!is.null(quadre)) {

    baseyear <- quadre[1]
    lastyear <- quadre[2]

    lrev <- lrev %>% filter(ANO.INICIO >= baseyear & ANO.INICIO <= lastyear |
                              ANO.FIM >= baseyear & ANO.FIM <= lastyear |
                              ANO.INICIO < baseyear & is.na(ANO.FIM))
  }

  return(lrev)

}
