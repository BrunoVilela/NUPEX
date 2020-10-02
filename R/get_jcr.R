# get_sjr <- function(artigos) {
#   jcr <- read_csv2("JCR/scimagojr 2019.csv")
#   revistas <- toupper(artigos$`TITULO-DO-PERIODICO-OU-REVISTA`)
#   revistas <- gsub("\\s*\\([^\\)]+\\)", "", revistas)
#   revistas <- gsub("&AMP;", "AND", revistas)
#
#   issn_match <- match(artigos$ISSN, jcr$Issn)
#   name_match <-
#     match(revistas,
#           toupper(jcr$Title))
#   matches <- ifelse(is.na(issn_match), name_match, issn_match)
#   return(jcr[matches, ])
# }
