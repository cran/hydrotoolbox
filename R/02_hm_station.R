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
#' \code{hydromet} subclass for store hydro-meteorological records.
#'
#' @description A suitable object for store your hydro-meteorological data.
#'
#' @slot hq water-height vs stream-discharge measurements.
#' @slot hw water level records.
#' @slot qh hourly mean river discharge.
#' @slot qd daily mean river discharge.
#' @slot qm monthly mean river discharge.
#' @slot qa annual river discharge.
#' @slot wspd wind speed.
#' @slot wdir wind direction.
#' @slot evap pan-evaporation.
#' @slot anem anemometer wind speed records (usually installed above the pan-evap tank).
#' @slot patm atmospheric pressure.
#' @slot rh relative humidity.
#' @slot tair air temperature (typically recorded at hourly time-step).
#' @slot tmax daily maximum recorded air temperature.
#' @slot tmin daily minimum recorded air temperature.
#' @slot tmean daily mean air temperature.
#' @slot tsoil soil temperature.
#' @slot precip total (snow and rain) precipitation records.
#' @slot rainfall liquid only precipitation measurements.
#' @slot swe snow water equivalent (typically recorded on snow pillows).
#' @slot hsnow snow height from ultrasonic devices.
#' @slot kin incoming short-wave radiation.
#' @slot kout outgoing short-wave radiation.
#' @slot lin incoming long-wave radiation.
#' @slot lout outgoing long-wave radiation.
#' @slot unvar reserved for non-considered variables.
#'
#' @return An hydromet_station class object.
#'
#' @importFrom methods new
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # create an hydromet station
#' hm_create(class_name = "station")
#'}
#'
hydromet_station <- setClass(
  # class name
  'hydromet_station',

  # slot definition
  slots = c(
    hq        = "ANY", # water-height vs stream-discharge measurements.
    hw        = "ANY", # water level records.
    qh        = "ANY", # hourly mean river discharge.
    qd        = "ANY", # daily mean river discharge.
    qm        = "ANY", # monthly mean river discharge.
    qa        = "ANY", # annual river disharge.
    wspd      = "ANY", # wind speed
    wdir      = "ANY", # wind direction.
    evap      = "ANY", # pan-evaporation.
    anem      = "ANY", # anemometer wind speed records
    patm      = "ANY", # atmospheric pressure.
    rh        = "ANY", # relative humidity.
    tair      = "ANY", # air temperature (typically recorded at hourly time-step).
    tmax      = "ANY", # daily maximum recorded air temperature.
    tmin      = "ANY", # daily minimum recorded air temperature.
    tmean     = "ANY", # daily mean air temperature.
    tsoil     = "ANY", # soil temperature.
    precip    = "ANY", # total (snow and rain) precipitation records.
    rainfall  = "ANY", # liquid only precipitation measurements.
    swe       = "ANY", # snow water equivalent (typically recorded on snow pillows).
    hsnow     = "ANY", # snow height from ultrasonic devices.
    kin       = "ANY", # incoming short-wave radiation.
    kout      = "ANY", # outgoing short-wave radiation.
    lin       = "ANY", # incoming long-wave radiation.
    lout      = "ANY", # outgoing long-wave radiation.
    unvar     = "ANY"  # unknown reserved for non-considered variables.
    ),

  # default values (optional)
  prototype = list(),

  # slot validation
  validity = function(object)
  {

    slot_names <- c("hq", "hw", "qh", "qd", "qm", "wspd", "wdir", "evap",
                    "anem", "patm", "rh", "tair", "tmax", "tmin", "tmean", "tsoil",
                    "precip", "rainfall", "swe", "hsnow", "kin", "kout", "lin", "lout",
                    "unvar")

    n_it <- length(slot_names)

    for(i in 1:n_it){
      slot2eval <- eval( parse( text = paste0("object", '@', slot_names[i]) )  )

      if( !is.null(slot2eval) ){
        # admito tibbles porque internamente
        # check_class() usa match()
        check_class(argument = slot2eval,
                    target = c("tbl_df", "tbl", "data.frame"),
                    arg_name = slot_names[i])

        # admito POSIXlt
        check_class(argument = slot2eval[ , 1, drop = TRUE],
                    target = c("Date", "POSIXct", "POSIXlt"),
                    arg_name = paste0(slot_names[i], "[ , 1]"))

      }

    }


#
#     # hq
#     if( dim(object@hq)[1]  != 0 ){
#       if(class(object@hq) != 'data.frame'){return('hq class must be a data.frame')}
#       if(class(object@hq[ , 1])[1] != 'POSIXct' & class(object@hq[ , 1])[1] != 'Date') {
#         return('hq[ , 1] class must be POSIXct or Date')}
#       if(class(object@hq[ , 2]) != 'numeric'){return('hq[ , 2] class must be numeric')}
#       if(class(object@hq[ , 3]) != 'numeric'){return('hq[ , 3] class must be numeric')}
#     }
#
#
#     # hw
#     if( dim(object@hw)[1]  != 0 ){
#       if(class(object@hw) != 'data.frame'){return('hw class must be a data.frame')}
#       if(class(object@hw[ , 1])[1] != 'POSIXct' & class(object@hw[ , 1])[1] != 'Date') {
#         return('hw[ , 1] class must be POSIXct or Date')}
#       if(class(object@hw[ , 2]) != 'numeric'){return('hw[ , 2] class must be numeric')}
#     }
#
#
#     # qh
#     if( dim(object@qh)[1]  != 0 ){
#       if(class(object@qh) != 'data.frame'){return('qh class must be a data.frame')}
#       if(class(object@qh[ , 1])[1] != 'POSIXct' & class(object@qh[ , 1])[1] != 'Date') {
#         return('qh[ , 1] class must be POSIXct or Date')}
#       if(class(object@qh[ , 2]) != 'numeric'){return('qh[ , 2] class must be numeric')}
#     }
#
#
#     # qd
#     if( dim(object@qd)[1]  != 0 ){
#       if(class(object@qd) != 'data.frame'){return('qd class must be a data.frame')}
#       if(class(object@qd[ , 1])[1] != 'POSIXct' & class(object@qd[ , 1])[1] != 'Date') {
#         return('qd[ , 1] class must be POSIXct or Date')}
#       if(class(object@qd[ , 2]) != 'numeric'){return('qd[ , 2] class must be numeric')}
#     }
#
#
#     # qm
#     if( dim(object@qm)[1]  != 0 ){
#       if(class(object@qm) != 'data.frame'){return('qm class must be a data.frame')}
#       if(class(object@qm[ , 1])[1] != 'POSIXct' & class(object@qm[ , 1])[1] != 'Date') {
#         return('qm[ , 1] class must be POSIXct or Date')}
#       if(class(object@qm[ , 2]) != 'numeric'){return('qm[ , 2] class must be numeric')}
#     }
#
#     # wspd
#     if( dim(object@wspd)[1]  != 0 ){
#       if(class(object@wspd) != 'data.frame'){return('wspd class must be a data.frame')}
#       if(class(object@wspd[ , 1])[1] != 'POSIXct' & class(object@wspd[ , 1])[1] != 'Date') {
#         return('wspd[ , 1] class must be POSIXct or Date')}
#       if(class(object@wspd[ , 2]) != 'numeric'){return('wspd[ , 2] class must be numeric')}
#     }
#
#
#     # wdir
#     if( dim(object@wdir)[1]  != 0 ){
#       if(class(object@wdir) != 'data.frame'){return('wdir class must be a data.frame')}
#       if(class(object@wdir[ , 1])[1] != 'POSIXct' & class(object@wdir[ , 1])[1] != 'Date') {
#         return('wdir[ , 1] class must be POSIXct or Date')}
#       if(class(object@wdir[ , 2]) != 'numeric'){return('wdir[ , 2] class must be numeric')}
#     }
#
#
#     # evap
#     if( dim(object@evap)[1]  != 0 ){
#       if(class(object@evap) != 'data.frame'){return('evap class must be a data.frame')}
#       if(class(object@evap[ , 1])[1] != 'POSIXct' & class(object@evap[ , 1])[1] != 'Date') {
#         return('evap[ , 1] class must be POSIXct or Date')}
#       if(class(object@evap[ , 2]) != 'numeric'){return('evap[ , 2] class must be numeric')}
#     }
#
#
#     # anem
#     if( dim(object@anem)[1]  != 0 ){
#       if(class(object@anem) != 'data.frame'){return('anem class must be a data.frame')}
#       if(class(object@anem[ , 1])[1] != 'POSIXct' & class(object@anem[ , 1])[1] != 'Date') {
#         return('anem[ , 1] class must be POSIXct or Date')}
#       if(class(object@anem[ , 2]) != 'numeric'){return('anem[ , 2] class must be numeric')}
#     }
#
#
#     # patm
#     if( dim(object@patm)[1]  != 0 ){
#       if(class(object@patm) != 'data.frame'){return('patm class must be a data.frame')}
#       if(class(object@patm[ , 1])[1] != 'POSIXct' & class(object@patm[ , 1])[1] != 'Date') {
#         return('patm[ , 1] class must be POSIXct or Date')}
#       if(class(object@patm[ , 2]) != 'numeric'){return('patm[ , 2] class must be numeric')}
#     }
#
#
#     # rh
#     if( dim(object@rh)[1]  != 0 ){
#       if(class(object@rh) != 'data.frame'){return('rh class must be a data.frame')}
#       if(class(object@rh[ , 1])[1] != 'POSIXct' & class(object@rh[ , 1])[1] != 'Date') {
#         return('rh[ , 1] class must be POSIXct or Date')}
#       if(class(object@rh[ , 2]) != 'numeric'){return('rh[ , 2] class must be numeric')}
#     }
#
#
#     # tair
#     if( dim(object@tair)[1]  != 0 ){
#       if(class(object@tair) != 'data.frame'){return('tair class must be a data.frame')}
#       if(class(object@tair[ , 1])[1] != 'POSIXct' & class(object@tair[ , 1])[1] != 'Date') {
#         return('tair[ , 1] class must be POSIXct or Date')}
#       if(class(object@tair[ , 2]) != 'numeric'){return('tair[ , 2] class must be numeric')}
#     }
#
#
#     # tmax
#     if( dim(object@tmax)[1]  != 0 ){
#       if(class(object@tmax) != 'data.frame'){return('tmax class must be a data.frame')}
#       if(class(object@tmax[ , 1])[1] != 'POSIXct' & class(object@tmax[ , 1])[1] != 'Date') {
#         return('tmax[ , 1] class must be POSIXct or Date')}
#       if(class(object@tmax[ , 2]) != 'numeric'){return('tmax[ , 2] class must be numeric')}
#     }
#
#
#     # tmin
#     if( dim(object@tmin)[1]  != 0 ){
#       if(class(object@tmin) != 'data.frame'){return('tmin class must be a data.frame')}
#       if(class(object@tmin[ , 1])[1] != 'POSIXct' & class(object@tmin[ , 1])[1] != 'Date') {
#         return('tmin[ , 1] class must be POSIXct or Date')}
#       if(class(object@tmin[ , 2]) != 'numeric'){return('tmin[ , 2] class must be numeric')}
#     }
#
#
#     # tmean
#     if( dim(object@tmean)[1]  != 0 ){
#       if(class(object@tmean) != 'data.frame'){return('tmean class must be a data.frame')}
#       if(class(object@tmean[ , 1])[1] != 'POSIXct' & class(object@tmean[ , 1])[1] != 'Date') {
#         return('tmean[ , 1] class must be POSIXct or Date')}
#       if(class(object@tmean[ , 2]) != 'numeric'){return('tmean[ , 2] class must be numeric')}
#     }
#
#
#     # tsoil
#     if( dim(object@tsoil)[1]  != 0 ){
#       if(class(object@tsoil) != 'data.frame'){return('tsoil class must be a data.frame')}
#       if(class(object@tsoil[ , 1])[1] != 'POSIXct' & class(object@tsoil[ , 1])[1] != 'Date') {
#         return('tsoil[ , 1] class must be POSIXct or Date')}
#       if(class(object@tsoil[ , 2]) != 'numeric'){return('tsoil[ , 2] class must be numeric')}
#     }
#
#
#     # precip
#     if( dim(object@precip)[1]  != 0 ){
#       if(class(object@precip) != 'data.frame'){return('precip class must be a data.frame')}
#       if(class(object@precip[ , 1])[1] != 'POSIXct' & class(object@precip[ , 1])[1] != 'Date') {
#         return('precip[ , 1] class must be POSIXct or Date')}
#       if(class(object@precip[ , 2]) != 'numeric'){return('precip[ , 2] class must be numeric')}
#     }
#
#
#     # rainfall
#     if( dim(object@rainfall)[1]  != 0 ){
#       if(class(object@rainfall) != 'data.frame'){return('rainfall class must be a data.frame')}
#       if(class(object@rainfall[ , 1])[1] != 'POSIXct' & class(object@rainfall[ , 1])[1] != 'Date') {
#         return('rainfall[ , 1] class must be POSIXct or Date')}
#       if(class(object@rainfall[ , 2]) != 'numeric'){return('rainfall[ , 2] class must be numeric')}
#     }
#
#
#     # swe
#     if( dim(object@swe)[1]  != 0 ){
#       if(class(object@swe) != 'data.frame'){return('swe class must be a data.frame')}
#       if(class(object@swe[ , 1])[1] != 'POSIXct' & class(object@swe[ , 1])[1] != 'Date') {
#         return('swe[ , 1] class must be POSIXct or Date')}
#       if(class(object@swe[ , 2]) != 'numeric'){return('swe[ , 2] class must be numeric')}
#     }
#
#
#     # hsnow
#     if( dim(object@hsnow)[1]  != 0 ){
#       if(class(object@hsnow) != 'data.frame'){return('hsnow class must be a data.frame')}
#       if(class(object@hsnow[ , 1])[1] != 'POSIXct' & class(object@hsnow[ , 1])[1] != 'Date') {
#         return('hsnow[ , 1] class must be POSIXct or Date')}
#       if(class(object@hsnow[ , 2]) != 'numeric'){return('hsnow[ , 2] class must be numeric')}
#     }
#
#
#     # kin
#     if( dim(object@kin)[1]  != 0 ){
#       if(class(object@kin) != 'data.frame'){return('kin class must be a data.frame')}
#       if(class(object@kin[ , 1])[1] != 'POSIXct' & class(object@kin[ , 1])[1] != 'Date') {
#         return('kin[ , 1] class must be POSIXct or Date')}
#       if(class(object@kin[ , 2]) != 'numeric'){return('kin[ , 2] class must be numeric')}
#     }
#
#
#     # lin
#     if( dim(object@lin)[1]  != 0 ){
#       if(class(object@lin) != 'data.frame'){return('lin class must be a data.frame')}
#       if(class(object@lin[ , 1])[1] != 'POSIXct' & class(object@lin[ , 1])[1] != 'Date') {
#         return('lin[ , 1] class must be POSIXct or Date')}
#       if(class(object@lin[ , 2]) != 'numeric'){return('lin[ , 2] class must be numeric')}
#     }
#
#
#     # unvar
#     if( dim(object@unvar)[1]  != 0 ){
#       if(class(object@unvar) != 'data.frame'){return('unvar class must be a data.frame')}
#       if(class(object@unvar[ , 1])[1] != 'POSIXct' & class(object@unvar[ , 1])[1] != 'Date') {
#         return('unvar[ , 1] class must be POSIXct or Date')}
#       if(class(object@unvar[ , 2]) != 'numeric'){return('unvar[ , 2] class must be numeric')}
#     }


    return(TRUE)
  },

  # set the inheritance for this class
  contains = 'hydromet'
)
