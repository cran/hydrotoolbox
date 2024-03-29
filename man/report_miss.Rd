% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_report_miss.R
\name{report_miss}
\alias{report_miss}
\title{Report \code{NA_real_} values inside a table.}
\usage{
report_miss(x, col_name = "all")
}
\arguments{
\item{x}{data frame with hydro-meteo data. First column is date and
the second the numeric vector to be reported.}

\item{col_name}{string vector with the column(s) name(s) to report.
By default the function will report all numeric columns.}
}
\value{
A list containing a data frame (one per \code{col_name})
with three columns: start-date, end-date and number of missing
time steps. In the last row of the table you will find the total
number of missing measurements (under "time_step" column). That's why
under start and end-date columns you will find \code{NA}.
}
\description{
Creates a data frame with reported dates and number of
times-step of missing or not recorded data.
}
\examples{

# read guido daily streamflow records
path <- system.file('extdata', 'snih_qd_guido.xlsx',
        package = 'hydrotoolbox')

# load raw data
qd_guido <-
  read_snih(path = path, by = 'day', out_name = 'q(m3/s)') \%>\%
  mov_avg(k = 5, out_name = 'q_smooth')

# get the data report
qd_guido \%>\%
  report_miss()



}
