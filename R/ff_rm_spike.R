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
#' Remove spikes
#'
#' @description Remove spikes and set their value as \code{NA_real_}.
#'
#' @param x data frame with class Date in the first column and
#' numeric on the others.
#' @param col_name string with column(s) name(s) where to apply the function.
#' @param out_name optional. String with new column(s) name(s). If you set it as \code{NULL},
#' the function will overwrite the original data frame.
#' @param tolerance numeric vector with the maximum tolerance between a number and its successor.
#' If you provide a single value it will be recycled.
#'
#' @return The same data frame but with the peaks removed.
#'
#' @export
#'
#' @examples
#'
#' # set path to file
#' path_file <- system.file('extdata', 'ianigla_cuevas.csv',
#'              package = 'hydrotoolbox')
#'
#' # read with default names
#' cuevas <- read_ianigla(path = path_file,
#'                        out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#'                                     'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#'                                     'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)') )
#'
#' # remove spikes in snow heigh series
#' cuevas %>%
#'   rm_spike(col_name = 'hsnow(cm)',
#'            out_name = 'hsnow',
#'            tolerance = 50) # 50 cm of snow its OK for this zone
#'
#'
rm_spike <- function(x,
                     col_name,
                     out_name = NULL,
                     tolerance){
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

  #* tolerance
  n_it <- length(col_name)
  check_class(argument = tolerance, target = c('numeric'), arg_name = 'tolerance')
  if( n_it > 1 & length(tolerance == 1) ){

    tolerance <- rep(tolerance, n_it)

  } else {

    check_cross(ref_arg = col_name, eval_arg = tolerance,
                arg_names = c('col_name', 'tolerance') )

  }


  #**************************
  #* function
  #**************************
  # get matrix
  mat_target <-
    x[ , col_name, drop = FALSE] %>%
    as.matrix()

  # get diff matrix
  mat_diff <-
    mat_target %>%
    apply(MARGIN = 2, FUN = diff) %>%
    abs()

  # get final matrix
  mat_rm <- mat_target

  for(i in 1:n_it){
    pos <- which(mat_diff[ , i] >= tolerance) + 1 # sum 1 because of first row in diff()

    mat_rm[pos, i] <- NA_real_
  }

  # set out name
  if( !is.null(out_name) ){
    #* use out_name
    colnames(mat_rm) <- out_name

    df_out <- x
    df_out[ , out_name] <- mat_rm

  } else {
    #* overwrite existing data frame

    df_out <- x

    df_out[ , col_name] <- mat_rm

  }

  # avoid the matrix issue
  df_out[ , -1] <- as.matrix( df_out[ , -1] )

  #* return
  return(df_out)

}
