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
#' Set user defined values
#'
#' @description Specify specific values between dates.
#'
#' @param x data frame with class \code{Date} or \code{POSIXct} in the first column and
#' numeric on the others.
#' @param col_name string with column(s) name(s) to set.
#' @param out_name optional. String with new column(s) name(s). If you set it as \code{NULL}, the
#' function will overwrite the original data frame.
#' @param value numeric vector with the numeric values to set between dates (\code{from} and
#' \code{to}). If you provide a number it will be recycled. When using a multiple dates (i.e.:
#' 'date' vector in \code{from} and \code{to}) use a list with a numeric vector inside each element.
#' @param from string vector for \code{'Date'} class or \code{POSIXct} class for date-time data
#' with the starting \code{Date}.
#' @param to string vector for \code{'Date'} class or \code{POSIXct} class for date-time data
#' with the ending \code{Date}.
#'
#' @return The same data frame but with the set numeric values between the dates.
#'
#' @export
#'
#' @examples
#'
#' # create a data frame
#' dates   <- seq.Date(from = as.Date('1990-01-01'), to = as.Date('1990-12-01'), by = 'm')
#' met_var <- runif(n = 12, 0, 10)
#'
#' met_table <- data.frame(dates, met_var)
#'
#' # set single value recycling
#' set_value(x = met_table, col_name = 'met_var', value = 10,
#'  from = '1990-01-01', to = '1990-06-01' )
#'
#' # set different periods
#' set_value(x = met_table, col_name = 'met_var', value = list(NA_real_, c(1, 2) ),
#'  from = c('1990-01-01', '1990-11-01'), to = c('1990-06-01', '1990-12-01') )
#'
#' # now set as new columns
#' set_value(x = met_table, col_name = 'met_var', out_name = 'met_set',
#'  value = list(NA_real_, c(1, 2) ),
#'  from = c('1990-01-01', '1990-11-01'),
#'   to = c('1990-06-01', '1990-12-01') )
#'
set_value <- function(x, col_name,
                      out_name = NULL, value,
                      from, to){
  #**************************
  #* conditionals
  #**************************
  #* x
  check_class(argument = x, target = 'data.frame', arg_name = 'x')
  check_class(argument = x[ , 1], target = c('Date', 'POSIXct') , arg_name = 'x[ , 1]')
  check_class(argument = c( as.matrix( x[ , -1] ) ),
              target = c('numeric') , arg_name = 'x[ , -1]')

  #* col_name
  check_class(argument = col_name, target = 'character', arg_name = 'col_name')
  check_string(argument = col_name,
               target = colnames(x)[-1],
               arg_name = 'col_name')

  #* out_name
  if( !is.null(out_name) ){

    check_class(argument = out_name, target = 'character', arg_name = 'out_name')

    guess <- which( match(x = out_name, table = colnames(x) ) >= 1 )
    if( length(guess) != 0){

      error_message <-
        paste0('Please check for ', out_name,
               ' argument. The following strins are forbidden: ',
               paste( colnames(x) , collapse = ', '), '.')

      stop( error_message, call. = FALSE )

    }

    check_cross(ref_arg = col_name,
                eval_arg = out_name,
                arg_names = c('col_name', 'out_name') )


  }

  #* from and to
  check_class(argument = from, target = c('character', 'POSIXct'), arg_name = 'from')
  check_class(argument = to, target = c('character', 'POSIXct'), arg_name = 'to')

  check_cross(ref_arg = from, eval_arg = to, arg_names = c('from', 'to') )

  n_it <- length(from) # to know if there are multiple periods

  #* value
  check_class(argument = value, target = c('numeric', 'list'), arg_name = 'value')
  if( n_it > 1 ){
    check_cross(ref_arg = from, eval_arg = value, arg_names = c('from and to', 'value') )
  }


  #**************************
  #* function
  #**************************
  #* get dates index
  if( class(x[ , 1]) == 'Date' ){

    ind_first <- match(x = as.Date( from ), table = x[ , 1])
    ind_last  <- match(x = as.Date( to ), table = x[ , 1])

  } else {
    # POSIXct class
    ind_first <- match(x = from, table = x[ , 1])
    ind_last  <- match(x = to, table = x[ , 1])

  }

  #* loop in vector dates
  #n_it <- length(ind_first)

  x_copy <- x

  if( n_it > 1) {
    # more than one period

    for(i in 1:n_it){
      #* get value[i]
      vect_num <- value[[i]]

      #* set values
      x_copy[ind_first[i]:ind_last[i], col_name] <- vect_num

    } # end for i

  } else {
    # single period
    x_copy[ind_first:ind_last, col_name] <- value

  }

  #* set out name
  if( !is.null(out_name) ){
    #* use out_name
    cl_names <- colnames(x)
    guess_nm <- match(x = col_name, table = cl_names)

    cl_names[guess_nm] <- out_name
    colnames(x_copy)   <- cl_names

    df_out <- merge(x = x, y = x_copy, all.x = TRUE)

  } else {
    #* overwrite existing data frame

    df_out <- x_copy

  }

  # return table
  return(df_out)

}
