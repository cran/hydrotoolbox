% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_cum_sum.R
\name{cum_sum}
\alias{cum_sum}
\title{Cumulative sum}
\usage{
cum_sum(x, col_name, out_name = NULL)
}
\arguments{
\item{x}{data frame or tibble with class \code{Date} or
\code{POSIX*} in the first column.}

\item{col_name}{string with column(s) name(s) where to apply the function.}

\item{out_name}{optional. String with new column(s) name(s). If you set it
as \code{NULL}, the function will overwrite the original table.}
}
\value{
The same table but with the new series.
}
\description{
The function supports \code{NA_real_} values. It could be very
useful when dealing with incomplete precipitation series.
}
\examples{
\dontrun{
# set path to file
path <- system.file('extdata', 'ianigla_cuevas.csv',
         package = 'hydrotoolbox')

# read the file and add the new column with cumulative precipitation
cuevas <-
  read_ianigla(path = path) \%>\%
  cum_sum(col_name = 'Precip_Total', out_name = 'p_cum')

# plot it
plot(x = cuevas[ , 'date', drop = TRUE],
     y = cuevas[ , 'p_cum', drop = TRUE],
     col = 'red', type = 'l',
     xlab = 'Date', ylab = 'Pcum(mm)')
}


}
