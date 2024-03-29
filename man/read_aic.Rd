% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_read_aic.R
\name{read_aic}
\alias{read_aic}
\title{Reads data from AIC}
\usage{
read_aic(
  path,
  by = "day",
  out_name = NULL,
  sheet = NULL,
  skip = 12,
  get_sheet = FALSE
)
}
\arguments{
\item{path}{path to the xlsx file.}

\item{by}{string with the time step of the series (e.g.: \code{'month', 'day', '6 hour',
'3 hour', '1 hour', '15 min' }). By default this argument is set to \code{'day'}. If
you set it as \code{'none'}, the function will ignore automatic gap filling.}

\item{out_name}{optional. String vector with user defined variable(s) column(s) name(s).}

\item{sheet}{optional. Sheet to read. Either a string (the name of a sheet), or an integer
(the position of the sheet). If neither argument specifies the sheet, defaults to the
 first sheet.}

\item{skip}{optional. Minimum number of rows to skip before reading anything, be it
column names or data. Leading empty rows are automatically skipped, so this is a lower bound.}

\item{get_sheet}{logical indicating whether you want to print available sheet names
(\code{TRUE}) in the file or not.}
}
\value{
A data frame with the data inside the xlsx file. Gaps between dates are filled with \code{NA_real_} and duplicated rows are eliminated automatically.
}
\description{
Reads excel files provided by the AIC.
}
\examples{

# This files are provided by AIC under legal agreement only.

}
