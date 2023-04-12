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
#' Interpolation
#'
#' @description This function applies interpolation to fill in missing
#'  (or non-recorded) values.
#'
#' @param x data frame with class \code{Date} or \code{POSIX*}
#' in the first column and numeric on the others.
#' @param col_name string with column name of the series to interpolate.
#' @param out_name optional. String with new column name. If you set
#' it as \code{NULL}, the function will overwrite the original data frame.
#' @param miss_table data frame with three columns: first and last date
#' of interpolation (first and second column respectively). The last and
#' third column, is of class \code{numeric} with the number of steps
#'  to interpolate. See \link{report_miss}.
#' @param threshold numeric variable with the maximum number of dates
#' in which to apply the interpolation.
#' @param method string with the interpolation method. In this version only
#' \code{'linear'} method is allowed.
#'
#' @return The same data frame but with interpolated values.
#'
#' @importFrom stats approx na.omit
#'
#' @export
#'
#' @examples
#'
#' # read cuevas station file
#' path <- system.file('extdata', 'ianigla_cuevas.csv',
#'         package = 'hydrotoolbox')
#'
#' cuevas <- read_ianigla(path = path)
#'
#' # get the miss_table
#' miss_data <- report_miss(x = cuevas, col_name = 'Irradiancia')[[1]]
#'
#' # apply interpolation function when gap is less than 3 hours
#' cuevas_interpo <- interpolate(x = cuevas,
#'                               col_name = 'Irradiancia',
#'                               out_name = 'kin_interpo',
#'                               miss_table = miss_data,
#'                               threshold = 3)
#'
#' report_miss(x = cuevas_interpo,
#'             col_name = c('Irradiancia', 'kin_interpo'))
#'
#'
interpolate <- function(x,
                        col_name, out_name = NULL,
                        miss_table, threshold,
                        method = 'linear'){
  #*+++++++++++++++
  #* conditionals
  #*+++++++++++++++
  #* x
  check_class(argument = x,
              target = c("tbl_df", "tbl", "data.frame"),
              arg_name = 'x')

  check_class(argument = x[ , 1, drop = TRUE],
              target = c('Date', 'POSIXct', 'POSIXlt'),
              arg_name = 'x[ , 1]')

  #* col_name
  check_class(argument = col_name,
              target = 'character',
              arg_name = 'col_name')

  check_string(argument = col_name,
               target = colnames(x)[-1],
               arg_name = 'col_name')

  check_length(argument = col_name,
               max_allow = 1,
               arg_name = 'col_name')

  check_class(argument = c( as.matrix( x[ , col_name, drop = FALSE] ) ),
              target = c('numeric'),
              arg_name = 'x[ , col_name]')


  #* out_name
  if( !is.null(out_name) ){

    check_class(argument = out_name,
                target = 'character',
                arg_name = 'out_name')

    guess <- which( match(x = out_name, table = colnames(x) ) >= 1 )
    if( length(guess) != 0){

      error_message <-
        paste0('Please check for ', out_name,
               ' argument. The following strings are forbidden: ',
               paste( colnames(x) , collapse = ', '), '.')

      stop( error_message, call. = FALSE )

    }

    check_cross(ref_arg = col_name,
                eval_arg = out_name,
                arg_names = c('col_name', 'out_name') )


  }

  #* miss_table
  check_class(argument = miss_table,
              target = c("tbl_df", "tbl", "data.frame"),
              arg_name = 'miss_table')

  check_class(argument = miss_table[ , 1, drop = TRUE],
              target = c('Date', 'POSIXct', 'POSIXlt'),
              arg_name = 'miss_table[ , 1]')

  check_class(argument = miss_table[ , 2, drop = TRUE],
              target = c('Date', 'POSIXct', 'POSIXlt'),
              arg_name = 'miss_table[ , 2]')

  check_class(argument = miss_table[ , 3],
              target = 'numeric',
              arg_name = 'miss_table[ , 3]')

    # remove last row
  miss_table <- na.omit(miss_table)


  #* threshold
  check_class(argument = threshold,
              target = 'numeric',
              arg_name = 'threshold')

  check_length(argument = threshold,
               max_allow = 1,
               arg_name = 'threshold')

  #* method
  check_class(argument = method,
              target = 'character',
              arg_name = 'method')

  check_string(argument = method,
               target = 'linear',
               arg_name = 'method')

  #*++++++++++++++++++
  #* function
  #*++++++++++++++++++
  #* rows to use
  miss_rows <- which( miss_table[ , 3] <= threshold )

  if( length(miss_rows) == 0){
    return('There are no gaps where to interpolate. Check for threshold argument!')
  }

  n_it <- length(miss_rows)
  out  <- x[ , col_name, drop = TRUE] # because I want the vector



  for(i in 1:n_it){
    i1 <- which( x[ , 1, drop = TRUE] == miss_table[miss_rows[i], 1] ) # first interpo position
    i2 <- which( x[ , 1, drop = TRUE] == miss_table[miss_rows[i], 2] ) # last interpo position

    # first and last position can not be interpolated
    if( i1 != 1 & i2 != nrow(x) ){

      j1 <- i1 - 1 # first to extract
      j2 <- i2 + 1 # last to extract

      var_aux <- x[j1:j2, col_name, drop = TRUE]

      aux <- approx(x = j1:j2, y = var_aux, xout = i1:i2)[[2]]

      out[i1:i2] <- aux

      rm(i, i1, i2, j1, j2, var_aux, aux)

    }



  }# end for i

  #* set out_name
  if( !is.null(out_name) ){
    #* use out_name
    out <- data.frame( x[ , 1, drop = TRUE], out )
    colnames(out) <- c( 'date', out_name)

    df_out <- merge(x = x, y = out, all.x = TRUE)

  } else {
    #* overwrite data frame
    x[ , col_name] <- out
    df_out <- x


  }

  #* return

  return(df_out %>% as_tibble())
}
