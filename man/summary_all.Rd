% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_all.R
\name{summary_all}
\alias{summary_all}
\title{Summary all lattes information}
\usage{
summary_all(lattes_data)
}
\arguments{
\item{lattes_data}{output result from get_lattes_folder function.}
}
\value{
A summary list
}
\description{
It summarizes the data obtained from get_lattes_folder
}
\examples{
folder_path <- system.file("lattes", package = "NUPEX")
lattes_folder_data <- get_lattes_folder(folder_path)
summary_lattes <- summary_all(lattes_folder_data)
}
