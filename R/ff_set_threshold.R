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
#' Set a threshold
#'
#' @description Set tolerable extreme values (maximum or minimum). Records greater or
#' equal than ('>=') or lesser or equal than ('<=') 'threshold' argument are set
#' to \code{NA_real_}.
#'
#' @param x data frame with class Date in the first column and
#' numeric on the others.
#' @param col_name string with column(s) name(s) where to apply the function.
#' @param out_name optional. String with new column(s) name(s). If you set it as \code{NULL},
#' the function will overwrite the original data frame.
#' @param threshold numeric vector with the threshold value(s).
#' If you provide a single value it will be recycled among \code{col_name} strings.
#' @param case string with either '>=' (greater or equal than) or '<=' (lesser or equal than)
#' symbol. Default string is '>='.
#'
#' @return The same data frame but with the threshold set.
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
#' # remove values higher than 1.50 meters
#' cuevas %>%
#'   set_threshold(col_name = 'hsnow(cm)',
#'                 out_name = 'hsnow_thres',
#'                 threshold = 150 )
#'
#'
set_threshold <- function(x,
                          col_name, out_name = NULL,
                          threshold, case = '>='){
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

  #* threshold
  n_it <- length(col_name)
  check_class(argument = threshold, target = c('numeric'), arg_name = 'threshold')
  if( n_it > 1 & length(threshold == 1) ){

    threshold <- rep(threshold, n_it)

  } else {

    check_cross(ref_arg = col_name, eval_arg = threshold,
                arg_names = c('col_name', 'threshold') )

  }

  #* case
  check_class(argument = case, target = c('character'), arg_name = 'case')
  if( n_it > 1 & length(case == 1) ){

    case <- rep(case, n_it)

  } else {

    check_cross(ref_arg = col_name, eval_arg = case,
                arg_names = c('col_name', 'case') )

  }


  #**************************
  #* function
  #**************************
  # get matrix
  mat_target <-
    x[ , col_name, drop = FALSE] %>%
    as.matrix()

  # get final matrix
  mat_thr <- mat_target

  for(i in 1:n_it){

    if(case[i] == '>='){

      pos <- which(mat_target[ , i] >= threshold[i])

      mat_thr[pos, i] <- NA_real_

    } else if(case[i] == '<='){

      pos <- which(mat_target[ , i] <= threshold[i])

      mat_thr[pos, i] <- NA_real_

    } else{

      stop( paste0('Only >= and <= are allowed for case argument!'),
            call. = FALSE)

      }


    } # end for i loop



  # set out name
  if( !is.null(out_name) ){
    #* use out_name
    colnames(mat_thr) <- out_name

    df_out <- x
    df_out[ , out_name] <- mat_thr

  } else {
    #* overwrite existing data frame

    df_out <- x

    df_out[ , col_name] <- mat_thr

  }

  # solve matrix issue
  df_out[ , -1] <- as.matrix( df_out[ , -1] )

  #* return
  return(df_out)

}
