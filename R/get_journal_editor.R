get_journal_editor <- function(xmls_list) {
  professional_activity <- get_professional_activity(xmls_list)
  if (is.null(professional_activity)) {
    return(NULL)
  } else {
    names(professional_activity)[names(professional_activity) ==
                                   "VINCULOS.OUTRO.VINCULO.INFORMADO"] <-
      "OUTRO.VINCULO.INFORMADO"

    refree <-
      professional_activity[professional_activity$OUTRO.VINCULO.INFORMADO == "Membro de corpo editorial",]
    if (nrow(refree) != 0) {
      nas <- apply(refree, 2, function(x) {
        !all(is.na(x))
      })
      nadas <-
        apply(refree, 2, function(x) {
          !all(x == "", na.rm = TRUE)
        })
      return(refree[, nas & nadas])
    } else {
      return(NULL)
    }
  }
}
