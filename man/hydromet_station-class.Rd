% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_hm_station.R
\docType{class}
\name{hydromet_station-class}
\alias{hydromet_station-class}
\alias{hydromet_station}
\title{\code{hydromet} subclass for store hydro-meteorological records.}
\value{
An hydromet_station class object.
}
\description{
A suitable object for store your hydro-meteorological data.
}
\section{Slots}{

\describe{
\item{\code{hq}}{water-height vs stream-discharge measurements.}

\item{\code{hw}}{water level records.}

\item{\code{qh}}{hourly mean river discharge.}

\item{\code{qd}}{daily mean river discharge.}

\item{\code{qm}}{monthly mean river discharge.}

\item{\code{qa}}{annual river discharge.}

\item{\code{wspd}}{wind speed.}

\item{\code{wdir}}{wind direction.}

\item{\code{evap}}{pan-evaporation.}

\item{\code{anem}}{anemometer wind speed records (usually installed above the pan-evap tank).}

\item{\code{patm}}{atmospheric pressure.}

\item{\code{rh}}{relative humidity.}

\item{\code{tair}}{air temperature (typically recorded at hourly time-step).}

\item{\code{tmax}}{daily maximum recorded air temperature.}

\item{\code{tmin}}{daily minimum recorded air temperature.}

\item{\code{tmean}}{daily mean air temperature.}

\item{\code{tsoil}}{soil temperature.}

\item{\code{precip}}{total (snow and rain) precipitation records.}

\item{\code{rainfall}}{liquid only precipitation measurements.}

\item{\code{swe}}{snow water equivalent (typically recorded on snow pillows).}

\item{\code{hsnow}}{snow height from ultrasonic devices.}

\item{\code{kin}}{incoming short-wave radiation.}

\item{\code{kout}}{outgoing short-wave radiation.}

\item{\code{lin}}{incoming long-wave radiation.}

\item{\code{lout}}{outgoing long-wave radiation.}

\item{\code{unvar}}{reserved for non-considered variables.}
}}

\examples{
\dontrun{
# create an hydromet station
hm_create(class_name = "station")
}

}
