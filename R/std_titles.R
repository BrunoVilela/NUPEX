# Function to correct html codes and typos in titles of published studies in
# journals, books, dissertations, etc

# Author: Domingos Cardoso

std_titles <- function(x) {

  require(textutils)
  require(stringi)
  require(tools)

  # Grabbing all specific columns of titles to be fixed/cleaned
  coltofix <- list()
  for (i in 7:length(names(x))) {
    coltofix[[i]] <- grepl("^TITULO$|^TITULO[-]DO[-]ARTIGO$|^TITULO[.]DO[.]TRABALHO$|^TITULO[-]DO[-]TRABALHO$|^TITULO-DO-LIVRO$|^TITULO[-]DO[-]CAPITULO[-]DO[-]LIVRO$",
                           names(x[[i]]))
  }

  # List of specific particles to be cleaned
  des <- c(" De ", " Das ", " Da ", " Dos ", " Do ", " No ",
           " Na ", " Em ", " E ", " Os ", " As ", " O ", " A ")
  des_ok <- c(" de ", " das ", " da ", " dos ", " do ", " no ",
              " na ", " em ", " e ", " os ", " as ", " o ", " a ")

  # Cleaning the titles with respect to HTML codes
  for (i in 7:length(names(x))) {
    if (length(coltofix[[i]]) >= 1) {
      if (any(coltofix[[i]])) {
        for (j in 1:length(which(coltofix[[i]]==T))) {
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- .cleanHtml(textutils::HTMLdecode(x[[i]][[names(x[[i]])[coltofix[[i]]][j]]]))

          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub(" \\\\", "....", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("(\\s){2,}", " ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\s[:]|[:];", ":", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\s,\\s", ", ", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("[(]\\s", "(", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\\s[)]", ")", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("[.]$|1$|&;lt;i&;gt;|&;lt;[/]i&;gt;|[-]$|[,]$|\\s$", "", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("&;", "&", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]]))
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\u0091\u0091|\u0092\u0092|\u0093|\u0094", "\"", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\u0092", "'", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub("\u0096", ",", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <- gsub(" ,", ",", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

          # Standardizing titles that are all in capital letters so as to make them with
          # just the initials of each word as uppercase
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <-
            as.character(x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

          gcaps <- grepl("([[:upper:]]){3,}.+\\w([[:upper:]]){3,}",
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
          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <-
            gsub("Et Al", "et al.", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

          x[[i]][[names(x[[i]])[coltofix[[i]]][j]]] <-
            gsub("sp[.]\\snov$", "sp. nov.", x[[i]][[names(x[[i]])[coltofix[[i]]][j]]])

        }
      }
    }
  }


  return(x)
}


# Decoding HTML codes and then removing the decoded characters
.cleanHtml <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

