# .collapse_names <- function(x) {
#   ifelse(length(x) > 1, paste(x, collapse = "; "), x)
# }
#
# get_altmetric <- function(artigos) {
#   require(rAltmetric)
#   require(plyr)
#   n_alt <- nrow(artigos)
#   result <- list()
#   for (i in 1:n_alt) {
#     alt_1 <-
#       try(altmetrics(doi = artigos$DOI[i]), silent = TRUE)
#     if (class(alt_1) == "try-error") {
#       result[[i]] <- tibble(doi = NA)
#     } else {
#       rem <- grep("history.", names(alt_1))
#       alt_1 <- alt_1[-rem]
#       result[[i]] <- tibble::as_tibble(lapply(alt_1, .collapse_names))
#     }
#   }
#   result <- do.call(plyr::rbind.fill, result)
#   return(tibble::as_tibble(result))
# }
