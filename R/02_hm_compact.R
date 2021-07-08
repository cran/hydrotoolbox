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
hydromet_compact <- setClass(
  # class name
  'hydromet_compact',

  # new slots
  slots = c(
    compact  = 'data.frame' # Date | ...(all the other numeric variables)
    ),

  # default values
  prototype = list(),

  #
  validity = function(object)
  {
    # slot validation
    if( dim(object@compact)[1] != 0 ){
      if(class(object@compact) != 'data.frame'){return('compact class must be data.frame')}
      if(class(object@compact[ , 1])[1] != 'Date'){
        if(class(object@compact[ , 1])[1] != 'POSIXct'){
          return('compact[ , 1] class must be Date or POSIXct')
        }
      }
      if(typeof( as.matrix(object@compact[ , -1, drop = FALSE])  ) != 'double'){return('compact[ , -1] elements must be of double type')}
    }


    return(TRUE)
  },

  # set the inheritance for this class
  contains = 'hydromet'
)
