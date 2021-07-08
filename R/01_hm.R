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
#' \code{hydromet} superclass object
#'
#' @description A suitable object for store basic information about an hydro-meteorological station.
#'
#' @slot id ANY. This is the ID assigned by the agency.
#' @slot agency string. The name of the agency (or institution) that provides the data of the station.
#' @slot station string. The name of the (hydro)-meteorological station.
#' @slot lat numeric. Latitude of the station.
#' @slot long numeric. Longitude of the station
#' @slot alt numeric. Altitude of the station.
#' @slot country string. Country where the station is located. Argentina is set as default value.
#' @slot province string. Name of the province where the station is located. Mendoza is set as default value.
#' @slot river string. Basin river's name.
#' @slot active logical. It indicates whether or not the station is currently operated. Default value is \code{TRUE}.
#' @slot basin_area numeric. The basin area (km2) of the catchment upstream of the gauge.
#' @slot basin_eff numeric. The effective area (km2) of the basin upstream of the gauge. In Canada, many basins have
#' variable contributing fractions. In these basins, the effective area of the basin contributes flow to the outlet at
#' least one year in two.
#' @slot other_1 ANY. It is the first free-to-fill slot in order to give you the chance to write extra information about your
#' hydro-met station.
#' @slot other_2 ANY. It is the second free-to-fill slot in order to give you the chance to write extra information about your
#' hydro-met station.
#'
#' @return A basic hydromet class object. This class is provided in order to set the meta-data of the station.
#'
#' @importFrom methods new
#'
#' @export
#'
hydromet <- setClass(
  # class name
  'hydromet',

  # slot definition
  slots = c(
    id       = 'ANY',
    agency   = 'character',
    station  = 'character',
    lat      = 'numeric',
    long     = 'numeric',
    alt      = 'numeric',
    country  = 'character',
    province = 'character',
    river    = 'character',
    active   = 'logical',
    basin_area = 'numeric',
    basin_eff  = 'numeric',
    other_1    = 'ANY',
    other_2    = 'ANY'

  ), # end slot

  # default values
  prototype = list(
    id       = NA_real_,
    agency   = NA_character_,
    station  = NA_character_,
    lat      = NA_real_,
    long     = NA_real_,
    alt      = NA_real_,
    country  = 'Argentina',
    province = 'Mendoza',
    river    = NA_character_,
    active   = TRUE,
    basin_area = NA_real_,
    basin_eff  = NA_real_,
    other_1    = NULL,
    other_2    = NULL

  ),# end prototype

  # conditionals
  validity = function(object){
    if(length(object@id) > 1){return('id should be of length one')}

    if(length(object@lat) > 1){return('lat should be of length one')}

    if(length(object@long) > 1){return('long should be of length one')}

    if(length(object@alt) > 1){return('alt should be of length one')}

    return(TRUE)

  }# end validity

)# end class
