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
#' Reads data from Explorador Climático (CR2 - Chile)
#'
#' @description Reads csv files downloaded from the CR2 web page as a data frame.
#'
#' @param path path to the csv file.
#' @param by string with the time step of the series (e.g.: \code{'month', 'day',
#'  '6 hour', '3 hour', '1 hour', '15 min' }). The default and unique possible
#'  value is \code{'day'}.
#' @param out_name optional. String vector with user defined variable(s)
#' column(s) name(s).
#'
#' @return A data frame with the data inside the csv file. Gaps between dates are
#'  filled with \code{NA_real_} and duplicated rows are eliminated automatically.
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#'
#' # list cr2 files
#' list.files( system.file('extdata', package = 'hydrotoolbox'), pattern = 'cr2' )
#'
#' # set path to file
#' path_tmax <- system.file('extdata', 'cr2_tmax_yeso_embalse.csv',
#'              package = 'hydrotoolbox')
#'
#' # read file with default colname
#' head( read_cr2(path = path_tmax) )
#'
#' # assign a column name
#' head( read_cr2(path = path_tmax, out_name = 'tmax(°C)') )
#'
read_cr2 <- function(path, by = 'day', out_name = NULL){
  #**************************
  #* conditionals
  #**************************
  #* path
  check_class(argument = path, target = 'character', arg_name = 'path')

  #* by
  check_class(argument = by, target = 'character', arg_name = 'by')
  check_string(argument = by, target = 'day', arg_name = 'by')
  check_length(argument = by, max_allow = 1, arg_name = 'by')

  #* out_name
  check_class(argument = out_name, target = 'character', arg_name = 'out_name')


  #**************************
  #* function
  #**************************
  #* read the file
  table_cr2 <- read.csv(file = path, header = TRUE, sep = ',', dec = '.')

  #* get date
  year  <- table_cr2[ , 1]
  month <- table_cr2[ , 2]
  day   <- table_cr2[ , 3]

  date_cr2 <- as.Date( paste0(year, '-', month, '-', day) )

  #* build data frame
  table_out <-
    data.frame(date = date_cr2, table_cr2[ , 4]) %>%
    fill_table(by = by) %>%
    unique.data.frame() # to remove dups

  #* out_name
  if( !is.null(out_name) ){

    check_length(argument = out_name, max_allow = 1, arg_name = 'out_name')

    colnames(table_out) <- c('date', out_name)

  } else{

    colnames(table_out) <- c('date', 'value')

  }

  #* return
  return(table_out)

}
