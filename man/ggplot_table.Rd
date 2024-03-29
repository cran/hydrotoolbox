% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_ggplot_table.R
\name{ggplot_table}
\alias{ggplot_table}
\title{Creates suitable table for \code{ggplot2}}
\usage{
ggplot_table(df, l_color, l_type, l_size, l_legend = NULL)
}
\arguments{
\item{df}{data frame with class Date or POSIXct in the first column and the variables to plot on the others.}

\item{l_color}{string vector with a color for each line (data frame column).}

\item{l_type}{string vector with the line type.}

\item{l_size}{numeric vector with the line size.}

\item{l_legend}{string vector with the user name for legend labels.}
}
\value{
A data frame ready to use in \code{ggplot2}.
}
\description{
Creates suitable table for \code{ggplot2}
}
\keyword{internal}
