#' Get all individual lattes information
#'
#' It summarizes individual XML lattes data into a list of tables
#' (in tibble format) separated by the type of information.
#'
#' @param file the name of XML file obtained from the lattes platform
#'
#' @return A list of tibbles
#'
#' @examples
#' path_lattes <- paste0(system.file("lattes", package = "NUPEX"),
#'  "/lattes10.xml")
#' lattes_data <- get_lattes(path_lattes)
#'
#' @export
#'
get_lattes <- function(file) {
  # Leia o cv lattes
  # xmls <- XML::xmlParse(file, encoding = "latin")
  # xmls_list <- XML::xmlToList(xmls)

  xmls_list <- XML::xmlToList(XML::xmlTreeParse(file,
                                   useInternal = TRUE))
  xmls_list <- rapply(xmls_list, function(x){Encoding(x) <- "UTF-8";x},
                      classes = "character",
                      how = "replace")

  # Dados básicos
  basic <- get_basic_data(xmls_list)
  idiom <- get_idioms(xmls_list)
  address <- get_address(xmls_list)
  areas <- get_areas(xmls_list)
  education <- get_education(xmls_list)
  courses <- get_courses(xmls_list)


  # Produção cientifica
  papers <- get_papers(xmls_list)
  papers_accepted <- get_papers_accepted(xmls_list)
  conference <- get_conference_pubs(xmls_list)
  books <- get_books(xmls_list)
  book_chapter <- get_books_chapters(xmls_list)
  text_media <- get_text_media(xmls_list)
  other_pubs <- get_other_publications(xmls_list)

  # Orientações
  supervision <- get_supervision(xmls_list)
  supervision_ongoing <- get_supervision_ongoing(xmls_list)

  # Participação em Banca
  defense_tribunal <- get_tribunal(xmls_list)
  other_comissions <- get_comission(xmls_list)

  # Participação em eventos
  events_participation <- get_conference_part(xmls_list)

  # Premios
  prizes <- get_prizes(xmls_list)

  # Produçao tecnica
  tech_prod <- get_tech_prod(xmls_list)
  software <- get_software(xmls_list)
  patent <- get_patent(xmls_list)
  tech_work <- get_tech_work(xmls_list)

  # Projetos
  projects <- get_projects(xmls_list)

  # Atuação profissional (revisao, editor, etc)
  journal_referee <- get_journal_referee(xmls_list)
  journal_editor <- get_journal_editor(xmls_list)
  professional_activity <- get_professional_activity(xmls_list)

  results <-  list(
    basic,
    education,
    courses,
    address,
    idiom,
    areas,
    papers,
    papers_accepted,
    conference,
    books,
    book_chapter,
    text_media,
    other_pubs,
    supervision,
    supervision_ongoing,
    defense_tribunal,
    other_comissions,
    events_participation,
    prizes,
    tech_prod,
    software,
    patent,
    tech_work,
    projects,
    journal_referee,
    journal_editor,
    professional_activity
  )
  names(results) <-
    c(
      "basic",
      'education',
      "courses",
      "address",
      "idiom",
      "areas",
      "papers",
      "papers_accepted",
      "conference",
      "books",
      "book_chapter",
      "text_media",
      "other_pubs",
      "supervision",
      "supervision_ongoing",
      "defense_tribunal",
      "other_comissions",
      "events_participation",
      "prizes",
      "tech_prod",
      "software",
      "patent",
      "tech_work",
      "projects",
      "journal_referee",
      "journal_editor",
      "professional_activity"
    )
  # if(.Platform$OS.type == "unix") {
  # } else {
  #   for (i in 1:length(results)) {
  #     if (!is.null(results[[i]])) {
  #       data.table::fwrite(results[[i]], "temp.csv")
  #       results[[i]] <- data.table::fread("temp.csv", encoding = "Latin-1")
  #       results[[i]] <- tibble::as_tibble(results[[i]])
  #       file.remove("temp.csv")
  #     }
  #   }
  # }
  return(results)
}
