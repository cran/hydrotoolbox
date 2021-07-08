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
#' Extract the slot
#'
#' @description Get the table (or metadata) that you want from an \code{hydromet} or \code{hydromet_XXX} class.
#'
#' @param obj an \code{hydromet} or \code{hydromet_XXX} class object.
#' @param slot_name string with slot to extract.
#'
#' @return The required data frame or metadata.
#'
#' @importFrom methods getSlots slotNames
#'
#' @export
#'
#' @examples
#'
#' # set path to file
#' path_file <- system.file('extdata', 'ianigla_cuevas.csv',
#'              package = 'hydrotoolbox')
#'
#' # read file
#' cuevas <-
#'      read_ianigla(path = path_file,
#'                   out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#'                              'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#'                               'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' ) )
#'
#' # create and set one the variables
#' hm_cuevas <-
#'       hm_create() %>%
#'       hm_set(tair = cuevas[ , c('date', 'tair(°C)')])
#'
#' # now extract the slot of air temperature
#' head( hm_get(obj = hm_cuevas, slot_name = 'tair') )
#'
setGeneric(name = 'hm_get',
           def = function(obj, slot_name = NA_character_)
           {
             standardGeneric('hm_get')
           }
)

#' @describeIn hm_get get method for generic hydromet object
# hydromet
setMethod(f = 'hm_get',
          signature = 'hydromet',
          definition = function(obj, slot_name = NA_character_)
          {

            #**************************
            #* conditionals
            #**************************
            #* check for classes
            check_class(argument = obj, target = 'hydromet', arg_name = 'obj')
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')

            #* check for argument consistency
            check_string(argument = slot_name, target = slotNames(obj), arg_name = 'slot_name')

            #* check for length
            check_length(argument = slot_name, max_allow = 1, arg_name = 'slot_name')


            #**************************
            #* function
            #**************************
            out <- eval( parse( text = paste0('obj', '@', slot_name) )  )

            return(out)
          }
)


#' @describeIn hm_get get method for station class
# station
setMethod(f = 'hm_get',
          signature = 'hydromet_station',
          definition = function(obj, slot_name = NA_character_)
          {
            #**************************
            #* conditionals
            #**************************
            #* check for classes
            check_class(argument = obj, target = 'hydromet_station', arg_name = 'obj')
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')

            #* check for argument consistency
            check_string(argument = slot_name, target = slotNames(obj), arg_name = 'slot_name')

            #* check for length
            check_length(argument = slot_name, max_allow = 1, arg_name = 'slot_name')


            #**************************
            #* function
            #**************************
            out <- eval( parse( text = paste0('obj', '@', slot_name) )  )

            return(out)

          }
)


#' @describeIn hm_get get method for compact class
# compact
setMethod(f = 'hm_get',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name = NA_character_)
          {
            #**************************
            #* conditionals
            #**************************
            #* check for classes
            check_class(argument = obj, target = 'hydromet_compact', arg_name = 'obj')
            check_class(argument = slot_name, target = 'character', arg_name = 'slot_name')

            #* check for argument consistency
            check_string(argument = slot_name, target = slotNames(obj), arg_name = 'slot_name')

            #* check for length
            check_length(argument = slot_name, max_allow = 1, arg_name = 'slot_name')


            #**************************
            #* function
            #**************************
            out <- eval( parse( text = paste0('obj', '@', slot_name) )  )

            return(out)

          }
)
