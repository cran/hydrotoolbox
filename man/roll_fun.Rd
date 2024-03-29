% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_roll_fun.R
\name{roll_fun}
\alias{roll_fun}
\title{Rolling functions}
\usage{
roll_fun(
  x,
  col_name = "last",
  k,
  pos = "c",
  FUN,
  ...,
  out_name = NULL,
  from = NULL,
  to = NULL
)
}
\arguments{
\item{x}{data frame (or tibble) with class \code{Date} or
\code{POSIX*} in the first column.}

\item{col_name}{string vector with the column(s) name(s) of the series to roll.
The default value uses the \code{'last'} column. Another single string choice
is to use \code{'all'}. Is important to keep in mind that this argument
\bold{commands}, so if you provide two columns
names, \code{k} and \code{pos} arguments must be of length two; if not the
single value will be recycled.}

\item{k}{numeric vector with the windows size. E.g.: \code{k = 5}.}

\item{pos}{string vector with the position of the windows: \itemize{
     \item 'c': center (default). The output value is in the middle of the window.
     \item 'l': left aligned. The output value is on the left, so the function weights
     the \code{(k - 1)} values at the right side.
     \item 'r': right aligned. The output value is on the right, so the function weights
     the \code{(k - 1)} values at the left side.
}}

\item{FUN}{the function to be applied.}

\item{...}{optional arguments to \code{FUN}.}

\item{out_name}{optional. String vector with new column names. If you set
it as \code{NULL} the function will overwrite the original series.}

\item{from}{optional. String value for \code{'Date'} class or \code{POSIX*}
class for date-time data containing the starting \code{Date}.}

\item{to}{optional. String value for \code{'Date'} class or \code{POSIX*}
class for date-time data containing the ending \code{Date}.}
}
\value{
The same table but with the rolling series.
}
\description{
It provides a generic function to rolling table columns.
Internally it is using \code{rollapplyr} from package \code{zoo}.
}
\examples{

# read guido daily streamflow records
path <- system.file('extdata', 'snih_qd_guido.xlsx',
        package = 'hydrotoolbox')

# read and apply the function
qd_guido <-
  read_snih(path = path, by = 'day', out_name = 'q(m3/s)') \%>\%
  roll_fun(k = 5, FUN = mean, na.rm = TRUE,
   out_name = 'q_smooth')



}
