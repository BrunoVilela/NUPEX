% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_lattes_fol.R
\name{get_lattes_folder}
\alias{get_lattes_folder}
\title{Get all lattes information}
\usage{
get_lattes_folder(folder_path, progress = TRUE)
}
\arguments{
\item{folder_path}{the path of the folder containing the XML files obtained from the lattes platform}

\item{progress}{if TRUE, progress bar is shown}
}
\value{
A list of tibbles
}
\description{
It summarizes multiple XML lattes data into a list of tables
(in tibble format) separated by the type of information.
}
\examples{
folder_path <- system.file("lattes", package = "NUPEX")
lattes_folder_data <- get_lattes_folder(folder_path, FALSE)

}
