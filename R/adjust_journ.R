# Auxiliary function to adjust/standardize names of journals

#' @author Domingos Cardoso
#'
#' @importFrom tools toTitleCase
#'
#' @export
#'

adjust_journ <- function(x) {

  if (any(class(x) == "data.frame")) {
    x <- list(x)
    names(x) <- "papers"
  }

  # Grabbing all specific columns of titles to be fixed/cleaned
  coltofix <- list()
  for (i in 1:length(names(x))) {

    coltofix[[i]] <- grepl("^TITULO[-]DO[-]PERIODICO[-]OU[-]REVISTA$|^NOME.INSTITUICAO$",
                           names(x[[i]]))
  }

  # List of specific particles to be cleaned
  des <- c(" De ", " Das ", " Da ", " Dos ", " Do ", " No ",
           " Na ", " Em ", " E ", " Os ", " As ", " O ", " A ")
  des_ok <- c(" de ", " das ", " da ", " dos ", " do ", " no ",
              " na ", " em ", " e ", " os ", " as ", " o ", " a ")

  # Cleaning the titles with respect to HTML codes
  for (i in 1:length(names(x))) {

    if (length(coltofix[[i]]) >= 1) {

      if (names(x)[i] == "papers"|
          names(x)[i] == "papers_accepted"|
          names(x)[i] == "journal_referee"|
          names(x)[i] == "journal_editor") {

        if (any(coltofix[[i]])) {

          for (j in 1:length(which(coltofix[[i]]==T))) {

            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("2[.][&][#]09;|RBCF[.]\\s|\\s[(].*|[.]$|[:].*|amp;",
                                                              "", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("M E T A B O L I S M C L I N I C A L A N D E X P E R I M E N T A L",
                                                              "METATOLISM CLINICAL AND EXPERIMENTAL", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

            # Standardizing titles that are all in capital letters so as to make them with
            # just the initials of each word as uppercase
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <-
              as.character(x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

            gcaps <- grepl("([[:upper:]]){2,}\\s([[:upper:]]){2,}|^([[:upper:]]){3,}$|([[:upper:]]){2,}[,]|([[:upper:]]){2,}\\s[&]|([[:upper:]]){2,}[-]|([[:upper:]]){2,}\\sE||([[:upper:]]){2,}[.]\\s",
                           x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

            if (any(gcaps)) {
              x[[i]][[names(x[[i]])[coltofix[[i]]][j]]][gcaps] <-
                tools::toTitleCase(gsub("\\b([a-z])", "\\U\\1",
                                        tolower(x[[i]][[names(x[[i]])[coltofix[[i]]][j]]][gcaps]),
                                        perl = TRUE))

              ggcaps <- grepl("[áéíóâêô][[:upper:]]",
                              x[[i]][[names(x[[i]])[coltofix[[i]]][j]]][gcaps])
              if(any(ggcaps)) {

                x[[i]][[names(x[[i]])[coltofix[[i]]][j]]][gcaps][ggcaps] <-
                  tools::toTitleCase(gsub("[áéíóâêô][[:upper:]]", "\\U\\1",
                                          tolower(x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])[gcaps][ggcaps], perl = TRUE))
              }

              for (l in seq_along(des)) {
                n <- 0
                n <- n + l
                x[[i]][[names(x[[i]])[coltofix[[i]]][j]]][gcaps] <-
                  gsub(des[l], des_ok[n], x[[i]][[names(x[[i]])[coltofix[[i]]][j]]][gcaps])
              }
            }

            # Further cleaning
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Peer J$",
                                                              "PeerJ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Peerj$|^Perrj$",
                                                              "PeerJ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^PLoS\\s|^Plos\\s",
                                                              "PLOS ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^PLOS ONE$",
                                                              "PLOS One", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("States of Ame$",
                                                              "States of America", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sjournal",
                                                              " Journal", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Bmc\\s",
                                                              "BMC ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Ssm\\s",
                                                              "SSM ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Rsc\\s",
                                                              "RSC ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sDna\\s",
                                                              " DNA ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sDna$",
                                                              " DNA", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Npj\\s",
                                                              "NPJ ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("Rna$",
                                                              "RNA", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Aids$",
                                                              "AIDS", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Aids\\s",
                                                              "AIDS ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sHiv$",
                                                              " HIV", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Isbt\\s",
                                                              "ISBT ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sNmr$",
                                                              " NMR", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Hiv\\s",
                                                              "HIV ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Ieee\\s",
                                                              "IEEE ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Ieee/Acm",
                                                              "IEEE/ACM", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("^Acs\\s",
                                                              "ACS ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sEt\\s",
                                                              " et ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("Antonie Van Leeuwenhoek International Journal of General and Molecular Micr",
                                                              "Antonie Van Leeuwenhoek", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sEmbo\\s",
                                                              " EMBO ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sFaseb\\s",
                                                              " FASEB ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sFebs\\s",
                                                              " FEBS ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sIioab\\s",
                                                              " IIOAB ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sUfmg$",
                                                              " UFMG", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sCfmv$",
                                                              " CFMV", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("Acta Botanica Brasílica",
                                                              "Acta Botanica Brasilica", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
            x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\sOnline\\s[-]\\s.*|[.]\\sJournal$.*",
                                                              "", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

          }
        }
      }
    }
  }

  return(x)
}
