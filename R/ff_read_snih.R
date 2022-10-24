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
#' Reads data from Servicio Nacional de Información Hídrica \href{https://back.argentina.gob.ar/obras-publicas/hidricas/base-de-datos-hidrologica-integrada}{(SNIH - Argentina)}
#'
#' @description Reads excel files downloaded from the SNIH web page as a data frame.
#'
#' @param path path to the xlsx file.
#' @param by string with the time step of the series (e.g.: \code{'month', 'day', '6 hour',
#' '3 hour', '1 hour', '15 min' }). If you set it as \code{'none'}, the function will
#' ignore automatic gap filling.
#' @param out_name optional. String vector with user defined variable(s) column(s) name(s).
#'
#' @return A data frame with the data inside the xlsx file. Gaps between dates are
#' filled with \code{NA_real_} and duplicated rows are eliminated automatically.
#'
#' @importFrom readxl read_xlsx
#'
#' @export
#'
#'
#' @examples
#'
#' # set path to file
#' path_file <- system.file('extdata', 'snih_qd_guido.xlsx', package = 'hydrotoolbox')
#'
#' # read daily streamflow with default column name
#' head( read_snih(path = path_file, by = 'day') )
#'
#' # now we use the function with column name
#' head( read_snih(path = path_file,  by = 'day', out_name = 'qd(m3/s)') )
#'
#'
read_snih <- function(path,
                      by,
                      out_name = NULL){
  #*++++++++++++++++++
  #* conditionals
  #*++++++++++++++++++
  #* path
  check_class(argument = path,
              target = 'character',
              arg_name = 'path')

  #* by
  check_class(argument = by,
              target = 'character',
              arg_name = 'by')

  check_length(argument = by,
               max_allow = 1,
               arg_name = 'by')

  #* out_name
  check_class(argument = out_name,
              target = 'character',
              arg_name = 'out_name')


  #*++++++++++++++++++
  #* function
  #*++++++++++++++++++
  #* read the file
  table_snih <-
    read_xlsx(path = path) %>%
    as.data.frame() # because returns a tibble

   # coerce
  n_col <- ncol(table_snih)
  for(i in 1:n_col){

    if(i == 1){
      table_snih[ , i] <- as.POSIXct(x = table_snih[ , i], tz = 'UTC',
                                     format = '%d/%m/%Y %H:%M')
    } else {
      table_snih[ , i] <- as.numeric( table_snih[ , i] )

    }


  }

    # set correct date format
  hour_flag <- length( grep(pattern = 'hour', x = by) )
  min_flag  <- length( grep(pattern = 'min', x = by) )

  if( hour_flag == 0 & min_flag == 0 ){

    table_snih[ , 1] <- as.Date( table_snih[ , 1] )

  }

  #* fill_table
  if(by == 'none'){

    table_fill <- table_snih

  } else {

    table_fill <- fill_table(x = table_snih, by = by)

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
