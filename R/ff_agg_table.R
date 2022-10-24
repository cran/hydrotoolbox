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
#' Aggregates a data frame to a larger time period
#'
#' @param x data frame or tibble with class \code{Date} or \code{POSIX*}
#'  in the first column.
#' @param col_name string with column(s) name(s) to aggregate.
#' @param fun string with supported aggregation function name (one per \option{col_name}):
#' \option{mean}, \option{min}, \option{max}, \option{sum}, \option{last} or \option{first}.
#' @param period string with the aggregation time-step: \option{hourly}, \option{daily},
#' \option{monthly}, \option{annually} or \option{climatic}. \bold{NOTE}: the \option{climatic}
#' option returns the all series annual statistics (\option{fun}).
#' @param out_name optional. String with the output column(s) name(s). Default values coerce the original
#' name plus the \option{fun} argument (e.g.: \code{tair_max}).
#' @param allow_na optional. Numeric value with the maximum allowed number of \code{NA_real_}
#' values. By default the function will not tolerate any \code{NA_real_} (and will return
#' \code{NA_real_} instead).
#' @param start_month optional. Numeric value defining the first month of the annual
#' period (it just make sense if \option{period} is either \option{annually} or
#' \option{climatic}). Default sets to 1 (January). \bold{NOTE}: keep in mind that if you
#' choose \option{climatic} as period you have to round off a complete year (e.g.:
#' \code{..., start_month = 6, end_month = 5, ...})
#' @param end_month optional. Numeric value defining the last month of the annual period
#' (it just make sense if \option{period} is either \option{annually} or
#' \option{climatic}). Default sets to 12 (December). \bold{NOTE}: keep in mind that if
#' you choose \option{climatic} as period you have to round off a complete year (e.g.:
#' \code{..., start_month = 6, end_month = 5, ...})
#'
#' @return A data frame with the Date and the aggregated variable(s).
#'
#' @export
#'
#'
#' @examples
#'
#' # set path to file
#' path <- system.file('extdata', 'snih_qd_guido.xlsx',
#'          package = 'hydrotoolbox')
#'
#' # read and load daily streamflow with default column name
#' guido_qd <- read_snih(path = path, by = 'day', out_name = 'q(m3/s)')
#'
#' # aggregate daily to monthly discharge
#' guido_q_month <- agg_table(x = guido_qd, col_name = 'q(m3/s)',
#'                           fun = 'mean', period = 'monthly',
#'                           out_name = 'qm(m3/s)')
#'
#' # suppose that we are interested on getting the annual maximum
#' # daily mean discharge for every hydrological year (since this
#' # station is located at the Mendoza River Basin ~32.9º S, we will
#' # consider that annual period starts on July)
#' guido_q_annual <- agg_table(x = guido_qd, col_name = 'q(m3/s)',
#'                             fun = 'max', period = 'annually',
#'                             out_name = 'qmax(m3/s)',
#'                             start_month = 7, end_month = 6)
#'
#' # now we want the mean, maximum and minimum monthly discharges
#' guido_q_stats <- agg_table(x = guido_qd, col_name = rep('q(m3/s)', 3),
#'                            fun = c('mean', 'max', 'min'),
#'                            period = 'monthly')
#'
#'
agg_table <- function(x, col_name, fun, period, out_name = NULL,
                      allow_na = 0, start_month = 1, end_month = 12){

  #*///////////////////
  #* conditionals
  #*///////////////////
  #* check for classes
  check_class(argument = x,
              target = c("tbl_df", "tbl", "data.frame"),
              arg_name = 'x')

  check_class(argument = x[ , 1, drop = TRUE],
              target = c("Date", "POSIXct", "POSIXlt"),
              arg_name = 'x[ , 1]')

  #check_class(argument = c( as.matrix( x[ , -1] ) ), target = 'numeric', arg_name = 'x[ , -1]' )

  check_class(argument = col_name,
              target = 'character',
              arg_name = 'col_name')

  check_class(argument = fun,
              target = 'character',
              arg_name = 'fun')

  check_class(argument = period,
              target = 'character',
              arg_name = 'period')

  if( !is.null(out_name) ){
    check_class(argument = out_name,
                target = 'character',
                arg_name = 'out_name')
  }

  check_class(argument = allow_na,
              target = 'numeric',
              arg_name = 'allow_na')

  if(is.na(allow_na)){stop( 'allow_na argument cannot be a NA value', call. = FALSE )}

  check_class(argument = start_month,
              target = 'numeric',
              arg_name = 'start_month')

  check_class(argument = end_month,
              target = 'numeric',
              arg_name = 'end_month')


  #* check for arguments consistency
  check_string(argument = col_name,
               target = colnames(x[ , -1, drop = FALSE]),
               arg_name = 'col_name' )

  check_string(argument = fun,
               target = c('mean', 'min', 'max', 'sum', 'last', 'first'),
               arg_name = 'fun')

  check_string(argument = period,
               target = c('hourly', 'daily', 'monthly',
                          'annually', 'climatic'),
               arg_name = 'period')

  check_numeric(argument = start_month,
                target = 1:12,
                arg_name = 'start_month')

  check_numeric(argument = end_month,
                target = 1:12,
                arg_name = 'end_month')

  #* check for arguments length
  check_length(argument = period,
               max_allow = 1,
               arg_name = 'period')

  check_length(argument = allow_na,
               max_allow = 1,
               arg_name = 'allow_na')

  check_length(argument = start_month,
               max_allow = 1,
               arg_name = 'start_month')

  check_length(argument = end_month,
               max_allow = 1,
               arg_name = 'end_month')

  #* cross validation
  check_cross(ref_arg = col_name,
              eval_arg = fun,
              arg_names = c('col_name', 'fun'))

   if( !is.null(out_name) ){
    check_cross(ref_arg = col_name,
                eval_arg = out_name,
                arg_names = c('col_name', 'out_name'))
  }

  #* miscellaneous
  if( period == 'climatic' ){
    if(start_month == 1 & end_month != 12){
      stop( 'If start_month is 1 (January), end_month should be 12 (December)', call. = FALSE )
    }

    if(start_month != 1 & (start_month - 1) != end_month ){
      stop( 'Remember that when using climatic as period, start_month and end_month arguments should round off a complete year!', call. = FALSE )
    }


  }


  #*///////////////////
  #* function
  #*///////////////////
  if(period == 'hourly'){
    out <- agg2hourly(df = x, col_name = col_name,
                      fun = fun, allow_na = allow_na)

  } else if(period == 'daily'){
    out <- agg2daily(df = x, col_name = col_name,
                     fun = fun, allow_na = allow_na)

  } else if(period == 'monthly'){
    out <- agg2monthly(df = x, col_name = col_name,
                       fun = fun, allow_na = allow_na)

  } else if(period == 'annually'){
    out <- agg2annually(df = x, col_name = col_name,
                        fun = fun, allow_na = allow_na,
                        start_month = start_month, end_month = end_month)

  } else if(period == 'climatic'){
    out <- agg2climatic(df = x, col_name = col_name,
                        fun = fun, allow_na = allow_na,
                        start_month = start_month, end_month = end_month)

  }

  # output column names
  if(period != 'annually'){
    if( is.null(out_name) ){
      colnames(out) <- c('date', paste0(col_name, '_', fun) )

    } else{
      colnames(out) <- c('date', out_name)
    }

  } else{
    if( is.null(out_name) ){
      colnames(out) <- c('begin', 'end', paste0(col_name, '_', fun) )

    } else{
      colnames(out) <- c('begin', 'end', out_name)
    }

  }


  # return table
  return(out)


}
