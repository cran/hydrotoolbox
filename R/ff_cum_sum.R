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
#' Cumulative sum
#'
#' @description The function supports \code{NA_real_} values. It could be very
#' useful when dealing with incomplete precipitation series.
#'
#' @param x data frame with class \code{Date} or \code{POSIXct} in the first column and
#' numeric on the others.
#' @param col_name string with column(s) name(s) where to apply the function.
#' @param out_name optional. String with new column(s) name(s). If you set it as \code{NULL},
#' the function will overwrite the original data frame.
#'
#' @return The same data frame but with the new series.
#'
#' @export
#'
#' @examples
#'
#' # set path to file
#' path <- system.file('extdata', 'ianigla_cuevas.csv',
#'          package = 'hydrotoolbox')
#'
#' # read the file and add the new column with cumulative precipitation
#' cuevas <-
#'   read_ianigla(path = path) %>%
#'   cum_sum(col_name = 'Precip_Total', out_name = 'p_cum')
#'
#' # plot it
#' plot(x = cuevas[ , 'date'], y = cuevas[ , 'p_cum'],
#'      col = 'red', type = 'l',
#'      xlab = 'Date', ylab = 'Pcum(mm)')
#'
#'
#'
cum_sum <- function(x,
                    col_name,
                    out_name = NULL){
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


  #**************************
  #* function
  #**************************

  #* get cumsum matrix
  x_cum <-
    subset(x = x, select = col_name) %>%
    as.matrix() %>%
    cumsum_matrix()


  #* out_name
  #* set out name
  if( !is.null(out_name) ){
    #* use out_name
    colnames(x_cum) <- out_name

    df_out <- x
    df_out[ , out_name] <- x_cum

  } else {
    #* overwrite existing data frame

    df_out <- x

    df_out[ ,col_name] <- x_cum

  }

  #* return
  return(df_out)


}
