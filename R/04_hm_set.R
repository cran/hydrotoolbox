# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#
#' Set the data of an \code{hydromet} object or its subclass
#'
#' @description With this method you can set (or change) an specific slot value (change the table).
#'
#' @param obj an \code{hydromet} or \code{hydromet_XXX} class object.
#' @param id ANY. This is the ID assigned by the agency.
#' @param agency character. The name of the agency (or institution) that provides the data of the station.
#' @param station character. The name of the (hydro)-meteorological station.
#' @param lat numeric. Latitude of the station.
#' @param long numeric. Longitude of the station
#' @param alt numeric. Altitute of the station.
#' @param country character. Country where the station is located. Argentina is set as default value.
#' @param province character. Name of the province where the station is located. Mendoza is set as default value.
#' @param river character. Basin river's name.
#' @param active logical. It indicates whether or not the station is currently operated. Default value is \code{TRUE}.
#' @param basin_area numeric. The basin area (km2) of the catchment upstream of the gauge.
#' @param basin_eff numeric. The effective area (km2) of the basin upstream of the gauge. In Canada, many basins have
#' variable contributing fractions. In these basins, the effective area of the basin contributes flow to the outlet at
#' least one year in two.
#' @param other_1 ANY. It is the first free-to-fill slot in order to give you the chance to write extra information about your
#' hydro-met station.
#' @param other_2 ANY. It is the second free-to-fill slot in order to give you the chance to write extra information about your
#' hydro-met station.
#' @param ... arguments to be passed to methods. They rely on the slots of the \code{obj} subclass.
#' @param hq water-height vs stream-discharge measurements.
#' @param hw water level records.
#' @param qh hourly mean river discharge.
#' @param qd daily mean river discharge.
#' @param qm monthly mean river discharge.
#' @param qa annual river discharge.
#' @param wspd wind speed.
#' @param wdir wind direction.
#' @param evap pan-evaporation.
#' @param anem anemometer wind speed records (usually installed above the pan-evap tank).
#' @param patm atmospheric pressure.
#' @param rh relative humidity.
#' @param tair air temperature (typically recorded at hourly time-step).
#' @param tmax daily maximum recorded air temperature.
#' @param tmin daily minimum recorded air temperature.
#' @param tmean daily mean air temperature.
#' @param tsoil soil temperature.
#' @param precip total (snow and rain) precipitation records.
#' @param rainfall liquid only precipitation measurements.
#' @param swe snow water equivalent (typically recorded on snow pillows).
#' @param hsnow snow height from ultrasonic devices.
#' @param kin incoming short-wave radiation.
#' @param kout outgoing short-wave radiation.
#' @param lin incoming long-wave radiation.
#' @param lout outgoing long-wave radiation.
#' @param unvar reserved for non-considered variables.
#' @param compact data frame with Date as first column. All other columns are hydro-meteorological variables.
#'
#' @return The hydromet object with the slots set.
#'
#' @importFrom methods callNextMethod validObject
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # create an hydro-met station
#' hm_guido <- hm_create(class_name = 'station')
#'
#' # assign altitude
#' hm_guido <- hm_set(obj = hm_guido, alt = 2480)
#'
#' # now we read streamflow - water height measurements
#' path_file <- system.file('extdata', 'snih_hq_guido.xlsx',
#' package = 'hydrotoolbox')
#' guido_hq  <- read_snih(path = path_file, by = 'none',
#'              out_name = c('h(m)', 'q(m3/s)',
#'                             'q_coarse_solid(kg/s)',
#'                             'q_fine_solid(kg/s)') )
#'
#' # set the new data frame
#'  # note: you can do it manually but using the hm_build() method
#'  #       is stromgly recommended
#' hm_guido <- hm_set(obj = hm_guido, hq = guido_hq)
#' hm_show(obj = hm_guido)
#'}
#'
## generic
setGeneric(name = 'hm_set',
           def = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                          lat = NULL, long = NULL, alt = NULL, country = NULL,
                          province = NULL, river = NULL, active = NULL,
                          basin_area = NULL, basin_eff = NULL,
                          other_1 = NULL, other_2 = NULL, ...)
           {
             standardGeneric('hm_set')
           }
)


#' @describeIn hm_set set method for generic object
## hydromet
setMethod(f = 'hm_set',
          signature = 'hydromet',
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                basin_area = NULL, basin_eff = NULL,
                                other_1 = NULL, other_2 = NULL, ...)
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }

            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }

            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }

            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }

            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }

            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }

            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }

            # country
            if(is.null(country) == FALSE){
              obj@country <- country
            }

            # province
            if(is.null(province) == FALSE){
              obj@province <- province
            }

            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }

            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }

            # basin_area
            if(is.null(basin_area) == FALSE){
              obj@basin_area <- basin_area
            }

            # basin_eff
            if(is.null(basin_eff) == FALSE){
              obj@basin_eff <- basin_eff
            }

            # other_1
            if(is.null(other_1) == FALSE){
              obj@other_1 <- other_1
            }

            # other_2
            if(is.null(other_2) == FALSE){
              obj@other_2 <- other_2
            }


            # check for valid input
            validObject(obj)

            # return
            return(obj)
          }
)


#' @describeIn hm_set set method for \code{station} object
## station
setMethod(f = 'hm_set',
          signature = 'hydromet_station',
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                basin_area = NULL, basin_eff = NULL,
                                other_1 = NULL, other_2 = NULL,
                                hq  = NULL, hw = NULL, qh = NULL, qd = NULL, qa = NULL,
                                qm = NULL, wspd = NULL, wdir = NULL, evap = NULL,
                                anem = NULL, patm = NULL, rh = NULL, tair = NULL,
                                tmax = NULL, tmin = NULL, tmean = NULL, tsoil = NULL,
                                precip = NULL, rainfall = NULL, swe = NULL, hsnow = NULL,
                                kin = NULL, kout = NULL, lin = NULL, lout = NULL,
                                unvar = NULL)
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }

            obj <- callNextMethod(obj)

            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }

            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }

            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }

            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }

            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }

            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }

            # country
            if(is.null(country) == FALSE){
              obj@country <- country
            }

            # province
            if(is.null(province) == FALSE){
              obj@province <- province
            }

            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }

            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }

            # basin_area
            if(is.null(basin_area) == FALSE){
              obj@basin_area <- basin_area
            }

            # basin_eff
            if(is.null(basin_eff) == FALSE){
              obj@basin_eff <- basin_eff
            }

            # other_1
            if(is.null(other_1) == FALSE){
              obj@other_1 <- other_1
            }

            # other_2
            if(is.null(other_2) == FALSE){
              obj@other_2 <- other_2
            }

            # h-q curve
            if(is.null(hq) == FALSE){
              colnames(hq) <- c('date', colnames(hq)[-1])
              obj@hq <- hq
            }

            # hw water height
            if(is.null(hw) == FALSE){
              colnames(hw) <- c('date', colnames(hw)[-1])
              obj@hw <- hw
            }

            # qh hourly discharge
            if(is.null(qh) == FALSE){
              colnames(qh) <- c('date', colnames(qh)[-1])
              obj@qh <- qh
            }

            # qd daily discharge
            if(is.null(qd) == FALSE){
              colnames(qd) <- c('date', colnames(qd)[-1])
              obj@qd <- qd
            }


            # qm monthly discharge
            if(is.null(qm) == FALSE){
              colnames(qm) <- c('date', colnames(qm)[-1])
              obj@qm <- qm
            }

            # qa annual discharge
            if(is.null(qa) == FALSE){
              colnames(qa) <- c('date', colnames(qa)[-1])
              obj@qa <- qa
            }



            # wspd wind speed
            if(is.null(wspd) == FALSE){
              colnames(wspd) <- c('date', colnames(wspd)[-1])
              obj@wspd <- wspd
            }


            # wdir wind direction
            if(is.null(wdir) == FALSE){
              colnames(wdir) <- c('date', colnames(wdir)[-1])
              obj@wdir <- wdir
            }


            # evap pan-evaporation
            if(is.null(evap) == FALSE){
              colnames(evap) <- c('date', colnames(evap)[-1])
              obj@evap <- evap
            }

            # anem
            if(is.null(anem) == FALSE){
              colnames(anem) <- c('date', colnames(anem)[-1])
              obj@anem <- anem
            }


            # patm
            if(is.null(patm) == FALSE){
              colnames(patm) <- c('date', colnames(patm)[-1])
              obj@patm <- patm
            }


            # rh
            if(is.null(rh) == FALSE){
              colnames(rh) <- c('date', colnames(rh)[-1])
              obj@rh <- rh
            }


            # tair
            if(is.null(tair) == FALSE){
              colnames(tair) <- c('date', colnames(tair)[-1])
              obj@tair <- tair
            }


            # tmax
            if(is.null(tmax) == FALSE){
              colnames(tmax) <- c('date', colnames(tmax)[-1])
              obj@tmax <- tmax
            }


            # tmin
            if(is.null(tmin) == FALSE){
              colnames(tmin) <- c('date', colnames(tmin)[-1])
              obj@tmin <- tmin
            }


            # tmean
            if(is.null(tmean) == FALSE){
              colnames(tmean) <- c('date', colnames(tmean)[-1])
              obj@tmean <- tmean
            }


            # tsoil
            if(is.null(tsoil) == FALSE){
              colnames(tsoil) <- c('date', colnames(tsoil)[-1])
              obj@tsoil <- tsoil
            }



            # precip
            if(is.null(precip) == FALSE){
              colnames(precip) <- c('date', colnames(precip)[-1])
              obj@precip <- precip
            }


            # rainfall
            if(is.null(rainfall) == FALSE){
              colnames(rainfall) <- c('date', colnames(rainfall)[-1])
              obj@rainfall <- rainfall
            }


            # swe
            if(is.null(swe) == FALSE){
              colnames(swe) <- c('date', colnames(swe)[-1])
              obj@swe <- swe
            }


            # hsnow
            if(is.null(hsnow) == FALSE){
              colnames(hsnow) <- c('date', colnames(hsnow)[-1])
              obj@hsnow <- hsnow
            }


            # kin
            if(is.null(kin) == FALSE){
              colnames(kin) <- c('date', colnames(kin)[-1])
              obj@kin <- kin
            }

            # kout
            if(is.null(kout) == FALSE){
              colnames(kout) <- c('date', colnames(kout)[-1])
              obj@kout <- kout
            }


            # lin
            if(is.null(lin) == FALSE){
              colnames(lin) <- c('date', colnames(lin)[-1])
              obj@lin <- lin
            }

            # lout
            if(is.null(lout) == FALSE){
              colnames(lout) <- c('date', colnames(lout)[-1])
              obj@lout <- lout
            }


            # unvar
            if(is.null(unvar) == FALSE){
              colnames(unvar) <- c('date', colnames(unvar)[-1])
              obj@unvar <- unvar
            }

            # object validation
            validObject(obj)

            # return
            return(obj)
          }
)


#' @describeIn hm_set set method for \code{compact} object
## compact
setMethod(f = 'hm_set',
          signature = 'hydromet_compact',
          definition = function(obj = NULL, id = NULL, agency = NULL, station = NULL,
                                lat = NULL, long = NULL, alt = NULL, country = NULL,
                                province = NULL, river = NULL, active = NULL,
                                basin_area = NULL, basin_eff = NULL,
                                other_1 = NULL, other_2 = NULL,
                                compact = NULL)
          {
            # obj
            if(is.null(obj) == TRUE){
              return('You must provide a valid obj argument')
            }

            obj <- callNextMethod(obj)

            # id
            if(is.null(id) == FALSE){
              obj@id <- id
            }

            # agency
            if(is.null(agency) == FALSE){
              obj@agency <- agency
            }

            # station
            if(is.null(station) == FALSE){
              obj@station <- station
            }

            # latitude
            if(is.null(lat) == FALSE){
              obj@lat <- lat
            }

            # longitude
            if(is.null(long) == FALSE){
              obj@long <- long
            }

            # altitude
            if(is.null(alt) == FALSE){
              obj@alt <- alt
            }

            # country
            if(is.null(country) == FALSE){
              obj@country <- country
            }

            # province
            if(is.null(province) == FALSE){
              obj@province <- province
            }

            # river
            if(is.null(river) == FALSE){
              obj@river <- river
            }

            # active
            if(is.null(active) == FALSE){
              obj@active <- active
            }

            # basin_area
            if(is.null(basin_area) == FALSE){
              obj@basin_area <- basin_area
            }

            # basin_eff
            if(is.null(basin_eff) == FALSE){
              obj@basin_eff <- basin_eff
            }

            # other_1
            if(is.null(other_1) == FALSE){
              obj@other_1 <- other_1
            }

            # other_2
            if(is.null(other_2) == FALSE){
              obj@other_2 <- other_2
            }

            # compact
            if(is.null(compact) == FALSE){
              colnames(compact) <- c('date', colnames(compact)[-1])
              obj@compact <- compact
            }

            # object validation
            validObject(obj)

            # return
            return(obj)
          }
)

