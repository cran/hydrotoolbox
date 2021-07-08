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
#' Creates an hydromet object.
#'
#' @description This function is the constructor of \code{hydromet} class and its subclass.
#'
#' @param class_name string with the name of the class. Valid arguments are: \code{hydromet}, \code{station} or \code{compact}.
#'
#' @return An S4 object of class \code{hydromet}.
#'
#' @importFrom methods new
#'
#' @export
#'
#' @examples
#' # create class 'hydromet'
#' hym_metadata <- hm_create(class_name = 'hydromet')
#'
#' # subclass 'station'
#' hym_station <- hm_create(class_name = 'station')
#'
#' # subclass 'compact'
#' hym_compact <- hm_create(class_name = 'compact')
#'
hm_create <- function(class_name = 'station'){
  #**************************
  #* conditionals
  #**************************
  #* check for classes
  check_class(argument = class_name, target = 'character', arg_name = 'class_name')

  #* check for length
  check_length(argument = class_name, max_allow = 1, arg_name = 'class_name')

  #* check for available strings
  check_string(argument = class_name, target = c('hydromet', 'station', 'compact'), arg_name = 'class_name')



  #**************************
  #* function
  #**************************

  if(class_name != 'hydromet'){
    out <- new(Class = paste0('hydromet_', class_name) )

  } else {
    out <- new(Class = 'hydromet')
  }

  # Salida
  return(out)

}
