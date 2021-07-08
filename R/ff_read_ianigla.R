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
#' Reads data from Sistema de Monitoreo Meteorológico de Alta Montaña (IANIGLA - Argentina)
#'
#' @description Reads csv files downloaded from the Sistema de Monitoreo Meteorológico de Alta Montaña
#' web page as a data frame.
#'
#' @param path path to the csv file.
#' @param by string with the time step of the series (e.g.: \code{'month', 'day', '6 hour', '3 hour',
#' '1 hour', '15 min' }). The default value is \code{'1 hour'}. If you set it as \code{'none'},
#' the function will ignore automatic gap filling.
#' @param out_name optional. String vector with user defined variable(s) column(s) name(s).
#'
#' @return A data frame with the data inside the csv file. Gaps between dates are filled with
#' \code{NA_real_} and duplicated rows are eliminated automatically.
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#'
#' @examples
#'
#' # set path to file
#' path_file <- system.file('extdata', 'ianigla_cuevas.csv',
#'              package = 'hydrotoolbox')
#'
#' # read with default names
#' head( read_ianigla(path = path_file) )
#'
#' # set column names
#' head(
#' read_ianigla(path = path_file,
#'              out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#'                            'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#'                            'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' ) )
#' )
#'
read_ianigla <- function(path, by = '1 hour', out_name = NULL){
  #**************************
  #* conditionals
  #**************************
  #* path
  check_class(argument = path, target = 'character', arg_name = 'path')

  #* by
  check_class(argument = by, target = 'character', arg_name = 'by')
  check_length(argument = by, max_allow = 1, arg_name = 'by')

  #* out_name
  check_class(argument = out_name, target = 'character', arg_name = 'out_name')


  #**************************
  #* function
  #**************************
  #* read the file
  Est <- YJday <- hh.mm.ss <- bat.Volts <- NULL # just for binding

  table_ianigla <-
    read.csv(file = path, header = TRUE, sep = ',', dec = '.') %>%
    subset(select = -c(Est, YJday, hh.mm.ss, bat.Volts) )

  table_ianigla[ , 'date'] <- as.POSIXct( table_ianigla[ , 'date'], tz = 'UTC' )

  #* fill_table
  if(by == 'none'){

    table_fill <- table_ianigla

  } else {

    table_fill <- fill_table(x = table_ianigla, by = by)

  }


  #* set out_names
  if( !is.null(out_name) ){

    check_length(argument = c('date', out_name),
                 max_allow = ncol(table_fill),
                 arg_name = 'out_name')

    colnames(table_fill) <- c('date', out_name)

  }

  #* return table
  return(table_fill)

}
