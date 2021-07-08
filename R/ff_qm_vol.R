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
#' Monthly river discharge [m3/s] to volume [hm3]
#'
#' @description Converts mean monthly river discharge [m3/s] to total volume discharge [hm3].
#'
#' @param x data frame with class Date in the first column and
#' numeric on the others.
#' @param col_name string with column(s) name(s) where to apply the function.
#' @param out_name optional. String with new column(s) name(s). If you set it as \code{NULL},
#' the function will overwrite the original data frame.
#'
#' @return The same data frame but with the total volume discharge.
#'
#' @importFrom lubridate month month<-
#'
#' @export
#'
#' @examples
#'
#' # read guido daily streamflow records
#' path <- system.file('extdata', 'snih_qd_guido.xlsx',
#'         package = 'hydrotoolbox')
#'
#' # read, aggregate the function to monthly resolution and get the volume
#' qm_guido <-
#'   read_snih(path = path, by = 'day', out_name = 'q(m3/s)') %>%
#'   agg_table(col_name = 'q(m3/s)', fun = 'mean', period = 'monthly',
#'             out_name = 'qm(m3/s)') %>%
#'   qm_vol(col_name = 'qm(m3/s)', out_name = 'vm(hm3)')
#'
#'
qm_vol <- function(x, col_name, out_name = NULL){
  #**************************
  #* conditionals
  #**************************
  #* x
  check_class(argument = x, target = 'data.frame', arg_name = 'x')
  check_class(argument = x[ , 1], target = c('Date') , arg_name = 'x[ , 1]')
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

  #**************************
  #* function
  #**************************
  n_it <- nrow(x)

  # get months
  month_plus <- x[ , 1]

  # make the magic with dates
  month(month_plus) <- month(month_plus) + 1

  days <- as.integer( format(month_plus - 1, format = '%d') )

  # calculate monthly discharge in hm3
  out <- x[ , col_name, drop = FALSE] * days * 0.0864

  # set out name
  if( !is.null(out_name) ){
    #* use out_name
    colnames(out) <- out_name

    df_out <- x
    df_out[ , out_name] <- out

  } else {
    #* overwrite existing data frame

    df_out <- x

    df_out[ , col_name] <- out

  }

  #* return
  return(df_out)
}







