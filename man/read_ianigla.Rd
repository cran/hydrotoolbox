% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_read_ianigla.R
\name{read_ianigla}
\alias{read_ianigla}
\title{Reads data from Sistema de Monitoreo Meteorológico de Alta Montaña (IANIGLA - Argentina)}
\usage{
read_ianigla(path, by = "1 hour", out_name = NULL)
}
\arguments{
\item{path}{path to the csv file.}

\item{by}{string with the time step of the series (e.g.: \code{'month', 'day', '6 hour', '3 hour',
'1 hour', '15 min' }). The default value is \code{'1 hour'}. If you set it as \code{'none'},
the function will ignore automatic gap filling.}

\item{out_name}{optional. String vector with user defined variable(s) column(s) name(s).}
}
\value{
A data frame with the data inside the csv file. Gaps between dates are filled with
\code{NA_real_} and duplicated rows are eliminated automatically.
}
\description{
Reads csv files downloaded from the Sistema de Monitoreo Meteorológico de Alta Montaña
web page as a data frame.
}
\examples{

# set path to file
path_file <- system.file('extdata', 'ianigla_cuevas.csv',
             package = 'hydrotoolbox')

# read with default names
head( read_ianigla(path = path_file) )

# set column names
head(
read_ianigla(path = path_file,
             out_name = c('tair(°C)', 'rh(\%)', 'patm(mbar)',
                           'p(mm)', 'wspd(km/hr)', 'wdir(°)',
                           'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' ) )
)

}
