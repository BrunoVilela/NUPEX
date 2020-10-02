get_journal_referee <- function(xmls_list) {
  professional_activity <- get_professional_activity(xmls_list)
  vinculo <- professional_activity$OUTRO.VINCULO.INFORMADO
  refree <- professional_activity[grep("Revisor de peri", vinculo), ]
  if (nrow(refree) != 0) {
    nas <- apply(refree, 2, function(x){!all(is.na(x))})
    nadas <- apply(refree, 2, function(x){!all(x == "", na.rm = TRUE)})
    return(refree[, nas & nadas])
  } else {
    return(NULL)
  }
}

