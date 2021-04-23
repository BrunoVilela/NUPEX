# Auxiliary function for manual adjustments of ISSNs in specific journals

#' @author Domingos Cardoso
#'
#' @importFrom tibble add_column
#'
#' @export
#'

adjust_ISSN <- function(papers){

  papers <- tibble::add_column(papers, periodico_temp = papers$TITULO.PERIODICO.OU.REVISTA, .after = "TITULO.PERIODICO.OU.REVISTA")
  papers <- tibble::add_column(papers, ISSN_temp = papers$ISSN, .after = "ISSN")
  papers <- tibble::add_column(papers, ESTRATO = NA, .after = "TITULO.PERIODICO.OU.REVISTA")
  papers$ISSN_temp <- gsub("(?<![0-9])0+", "", papers$ISSN_temp, perl = TRUE)

  papers$TITULO.PERIODICO.OU.REVISTA <- NA
  papers$ISSN <- gsub("^$", NA, papers$ISSN)
  papers$ISSN <- as.character(papers$ISSN)
  papers$ISSN_temp <- as.character(papers$ISSN_temp)
  papers$ISSN_temp <- gsub("^$", NA, papers$ISSN_temp)
  papers$ESTRATO <- as.character(papers$ESTRATO)
  papers$periodico_temp <- as.character(papers$periodico_temp)

  # Initial cleaning of journals without ISSN
  jj <- c("Kew Bulletin", "Kew Bull.", "Kew Bull")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "755974"
  }

  jj <- c("Acta Botanica Malacitana")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "2109506"
  }

  jj <- c("Biochemical Systematic and Ecology")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "3051978"
  }

  jj <- c("Harvard Papers in Botany")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "19382944"
  }

  jj <- c("Boletim de Botanica da Universidade de Sao Paulo",
          "Bolm. Botanica Univ. Sao Paulo",
          "Bolm Botanica Univ Sao Paulo",
          "Bolm Botanica Univ. Sao Paulo",
          "Bol. Bot. Univ. Sao Paulo",
          "Boletim de BotÃ¢nica da Universidade de SÃ£o Paulo",
          "Boletim de Botânica da Universidade de São Paulo",
          "Boletim de BotÃ¢nica da Universidade de SÃ£o",
          "Boletim de Botânica da Universidade de São",
          "Bolm. Botanica")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "3022439"
  }

  jj <- c("Taxon")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "400262"
  }

  jj <- c("Plant Systematics and Evolution",
          "Plant Syst. Evol")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "3782697"
  }

  jj <- c("Novon")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "10553177"
  }

  jj <- c("Revta. Brasil. Bot.")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "1008404"
  }

  jj <- c("Aquatic Botany")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "3043770"
  }

  jj <- c("Ann. Miss. Bot. Gdn.",
          "Ann. Miss. Bot. Gdn")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "266493"
  }

  jj <- c("Acta Bot. Bras.",
          "Acta. Bot. Bras.")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "1023306"
  }

  jj <- c("Sitientibus",
          "Sitientibus sÃ©rie CiÃªncia BiolÃ³gicas",
          "Sitientibus série Ciência Biológicas")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "22384103"
  }

  jj <- c("Biotropica")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "63606"
  }

  jj <- c("Conservation Biology")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "8888892"
  }

  jj <- c("Quimica Nova")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "1004042"
  }

  jj <- c("Revista da Universidade de Garulhos")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "19817428"
  }

  jj <- c("Cien. Cult.")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "96725"
  }

  jj <- c("Botanical Journal of the Linnean Society")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "244074"
  }

  jj <- c("Revista de Biologia Tropical")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "347744"
  }

  jj <- c("Boletim do Museu Paraense Emílio Goeldi")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$ISSN_temp)] %in% i
    papers$ISSN_temp[is.na(papers$ISSN_temp)][a] <- "347744"
  }

  jj <- c("Genetics and Molecular Biology")
  for(i in jj){
    a <- vector()
    a <- papers$periodico_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "14154757"
    papers$ISSN[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "14154757"
  }

  # Correcting ISSNs that are in CAPES but differently
  jj <- c("15196097",
          "1018841",
          "19845812",
          "22384123")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "22384103"
  }

  jj <- c("1006762")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "18069088"
  }

  jj <- c("22368906")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "732877"
  }

  jj <- c("16760603",
          "16760611")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "16786424"
  }

  jj <- c("102695X")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "1981528X"
  }

  jj <- c("23582847")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "18095348"
  }

  jj <- c("445967")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "18094392"
  }

  jj <- c("23819685")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "23819677"
  }

  jj <- c("6364414X")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "15482324"
  }

  jj <- c("16790073")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "21783675"
  }

  jj <- c("23405074")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "2109506"
  }

  jj <- c("23169052")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "3022439"
  }

  jj <- c("14429985")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "14429993"
  }

  jj <- c("21568456")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "21568502"
  }

  jj <- c("373840X")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "25257412"
  }

  jj <- c("21598347")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "269249X"
  }

  jj <- c("19843909")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "18096875"
  }

  jj <- c("21582742")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "21582750"
  }

  jj <- c("22364420")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "1025333"
  }

  jj <- c("23176660")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "96725"
  }

  jj <- c("1024698")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "19826621"
  }

  jj <- c("18093647")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "19832605"
  }

  jj <- c("19804849")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "16792343"
  }

  jj <- c("14133210")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "19817428"
  }

  jj <- c("15168913")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "16784324"
  }

  jj <- c("22253610")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "22237054"
  }

  jj <- c("15160572")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "1983084X"
  }

  jj <- c("1024175")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "21782229"
  }

  jj <- c("23144149")
  for(i in jj){
    a <- vector()
    a <- papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)] %in% i
    papers$ISSN_temp[is.na(papers$TITULO.PERIODICO.OU.REVISTA)][a] <- "23144157"
  }

  return(papers)

}
