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
#' \code{hydromet} subclass for compact data
#'
#' @description This subclass is useful for storing in a single data frame ready to use hydro-meteorological series or many variables of the same kind (e.g. lets say precipitation series).
#'
#' @slot compact data.frame with Date as first column (class 'Date' or 'POSIXct'). All other columns are the numeric hydro-meteorological variables (double). This subclass was though to join in a single table ready to use data (e.g. in modeling). You can also use it to put together variables of the same kind (e.g. precipitation records) to make some regional analysis.
#'
#' @importFrom methods new
#
#' @return A hydromet_compact class object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # create an compact station
#' hm_create(class_name = "compact")
#'}
hydromet_compact <- setClass(
  # class name
  'hydromet_compact',

  # new slots
  slots = c(
    compact  = "ANY" # Date | ...
    ),

  # default values
  prototype = list(),

  #
  validity = function(object)
  {
    # slot validation

    # new validation
    if( !is.null(object@compact) ){
      # admito tibbles porque internamente
      # check_class() usa match()
      check_class(argument = object@compact,
                  target = c("tbl_df", "tbl", "data.frame"),
                  arg_name = "compact")

      # admito POSIXlt
      check_class(argument = object@compact[ , 1, drop = TRUE],
                  target = c("Date", "POSIXct", "POSIXlt"),
                  arg_name = "compact[ , 1]")

    }

    # old validation
    # if( dim(object@compact)[1] != 0 ){
    #   # check_class(argument = object@compact,
    #   #             target = "data.frame",
    #   #             arg_name = "compact")
    #   #
    #   # check_class(argument = object@compact[ , 1, drop = TRUE],
    #   #             target = c("Date", "POSIXct", "POSIXlt"),
    #   #             arg_name = "compact[ , 1]")
    #   if(class(object@compact) != 'data.frame'){return('compact class must be data.frame')}
    #   if(class(object@compact[ , 1])[1] != 'Date'){
    #     if(class(object@compact[ , 1])[1] != 'POSIXct'){
    #       return('compact[ , 1] class must be Date or POSIXct')
    #     }
    #   }
    #   if(typeof( as.matrix(object@compact[ , -1, drop = FALSE])  ) != 'double'){
    #     return('compact[ , -1] elements must be of double type')}
    # }


    return(TRUE)
  },

  # set the inheritance for this class
  contains = "hydromet"
)
