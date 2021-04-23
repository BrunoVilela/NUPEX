#' Produce total of supervisions
#'
#' @author Domingos Cardoso
#'
#' @description It uses the list (data frame format) of ongoing and finished
#' supervisions derived by \code{\link{listsuperv}} so as to produce a summary
#' of total supervisions
#'
#' @param supervdata table (data frame format) derived by \code{\link{listsuperv}}
#' function, which has to be used so as to get the total number of granted projects
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
#' lsuperv <- listsuperv(lattes_data,
#'                       quadre = c(2017, 2020))
#' tsuperv <- totalsuperv(lsuperv,
#'                        lattes_data,
#'                        collabs = collaborators)
#'
#' path_lattes_folder <- system.file("lattes", package = "NUPEX")
#' lattes_folder_data <- get_lattes_folder(path_lattes_folder)
#' lsuperv <- listsuperv(lattes_folder_data,
#'                       quadre = c(2017, 2020))
#' collaborators <- c("Suzana Telles da Cunha Lima",
#'                    "Cláudia Dias de Santana")
#' tsuperv <- totalsuperv(lsuperv,
#'                        lattes_folder_data,
#'                        collabs = collaborators)
#'}
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#'
#' @export
#'

totalsuperv <- function(supervdata,
                        lattesdata,
                        collabs = NULL) {

  ori <- listID(lattesdata,
                collabs = collabs)

  # Create new columns using all names from a vector
  newcols <- c("MESTRADO_andame", "MESTRADO_conclu", "DOUTORADO_andame", "DOUTORADO_conclu",
               "POS_DOUTORADO_andame", "POS_DOUTORADO_conclu", "MONOGRAFIA_ESPECIALIZACAO_andame",
               "MONOGRAFIA_ESPECIALIZACAO_conclu", "INICIACAO_CIENTIFICA_andame",
               "INICIACAO_CIENTIFICA_conclu", "MONOGRAFIA_GRADUACAO_andame",
               "MONOGRAFIA_GRADUACAO_conclu", "OUTRA_NATUREZA_andame",
               "OUTRA_NATUREZA_conclu")
  ori[, newcols] <- NA

  for (i in seq_along(unique(ori$NOME))) {
    g <- ori$NOME %in% unique(supervdata$NOME)
    if (any(g[i])) {
      j <- unique(ori$NOME)[i]
      temp <- supervdata[supervdata$NOME == j, ]

      if (any(unique(temp$NATUREZA) == "Dissertação de mestrado", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Dissertação de mestrado")
        ori$MESTRADO_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Dissertação de mestrado", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Dissertação de mestrado")
        ori$MESTRADO_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

      temp <- supervdata[supervdata$NOME == j, ]
      if (any(unique(temp$NATUREZA) == "Tese de doutorado", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Tese de doutorado")
        ori$DOUTORADO_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Tese de doutorado", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Tese de doutorado")
        ori$DOUTORADO_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

      temp <- supervdata[supervdata$NOME == j, ]
      if (any(unique(temp$NATUREZA) == "Supervisão de pós-doutorado", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Supervisão de pós-doutorado")
        ori$POS_DOUTORADO_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Supervisão de pós-doutorado", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Supervisão de pós-doutorado")
        ori$POS_DOUTORADO_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

      temp <- supervdata[supervdata$NOME == j, ]
      if (any(unique(temp$NATUREZA) == "Monografia de conclusão de curso de aperfeiçoamento especialização", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Monografia de conclusão de curso de aperfeiçoamento especialização")
        ori$MONOGRAFIA_ESPECIALIZACAO_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Monografia de conclusão de curso de aperfeiçoamento especialização", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Monografia de conclusão de curso de aperfeiçoamento especialização")
        ori$MONOGRAFIA_ESPECIALIZACAO_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

      temp <- supervdata[supervdata$NOME == j, ]
      if (any(unique(temp$NATUREZA) == "Iniciação Científica", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Iniciação Científica")
        ori$INICIACAO_CIENTIFICA_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Iniciação Científica", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Iniciação Científica")
        ori$INICIACAO_CIENTIFICA_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

      temp <- supervdata[supervdata$NOME == j, ]
      if (any(unique(temp$NATUREZA) == "Trabalho de conclusão de curso de graduação", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Trabalho de conclusão de curso de graduação")
        ori$MONOGRAFIA_GRADUACAO_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Trabalho de conclusão de curso de graduação", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Trabalho de conclusão de curso de graduação")
        ori$MONOGRAFIA_GRADUACAO_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

      temp <- supervdata[supervdata$NOME == j, ]
      if (any(unique(temp$NATUREZA) == "Orientação de outra natureza", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Orientação de outra natureza")
        ori$OUTRA_NATUREZA_andame[i] <- length(temp$NATUREZA[temp$STATUS == "EM ANDAMENTO"])
      }
      if (any(unique(temp$NATUREZA) == "Orientação de outra natureza", na.rm = TRUE)) {
        temp <- temp %>% filter(NATUREZA == "Orientação de outra natureza")
        ori$OUTRA_NATUREZA_conclu[i] <- length(temp$NATUREZA[temp$STATUS == "CONCLUIDA"])
      }

    }
  }

  # Fill in all NA by 0
  ori <- as.data.frame(ori)
  for (i in 4:length(names(ori))) {
    ori[,i][is.na(ori[,i])] <- 0
  }

  return(ori)

}
