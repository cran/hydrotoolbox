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
#' Snow Water Equivalent to melt or snowfall
#'
#' @description Derive melt or snowfall series from snow water
#' equivalent measurements (snow pillows measurements).
#'
#' @param x data frame or tibble with class Date or POSIX* in the
#' first column.
#' @param col_name string with column(s) name(s) where to apply the function.
#' @param out_name optional. String with new column(s) name(s). If you
#' set it as \code{NULL}, the function will overwrite the original table.
#' @param case string vector with "sf" (meaning snowfall) or "m" (meaning melt).
#'
#' @return The same data frame but with the derived series.
#'
#' @export
#'
#' @examples
#'
#' # set path to file
#' path_file <- system.file('extdata', 'dgi_toscas.xlsx',
#'              package = 'hydrotoolbox')
#'
#' # swe table
#' swe_toscas <- read_dgi(path = path_file,
#'                        sheet = 'swe',
#'                        out_name = 'swe(mm)')
#'
#' # add melt and snowfall
#' swe_toscas <-
#'   swe_toscas %>%
#'   swe_derive(col_name = rep('swe(mm)', 2),
#'              out_name = c('melt(mm)', 'snowfall(mm)'),
#'              case = c('m', 'sf') )
#'
#'
swe_derive <- function(x,
                       col_name,
                       out_name = NULL,
                       case){
  #*++++++++++++++++++
  #* conditionals
  #*++++++++++++++++++
  #* x
  check_class(argument = x,
              target = c("tbl_df", "tbl", "data.frame"),
              arg_name = 'x')

  check_class(argument = x[ , 1, drop = TRUE],
              target = c("Date", "POSIXct", "POSIXlt"),
              arg_name = 'x[ , 1]')

  # check_class(argument = c( as.matrix( x[ , -1] ) ),
  #             target = c('numeric') , arg_name = 'x[ , -1]')

  #* col_name
  check_class(argument = col_name,
              target = 'character',
              arg_name = 'col_name')

  check_string(argument = col_name,
               target = colnames(x)[-1],
               arg_name = 'col_name')

  check_class(argument = c( as.matrix( x[ , col_name] ) ),
              target = c('numeric'),
              arg_name = 'x[ , col_name]')

  n_it <- length(col_name)

  #* out_name
  if( !is.null(out_name) ){

    check_class(argument = out_name,
                target = 'character',
                arg_name = 'out_name')

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


  #* case
  check_class(argument = case,
              target = c('character'),
              arg_name = 'case')

  check_string(argument = case,
               target = c('sf', 'm'),
               arg_name = 'case')

  if( n_it > 1 & length(case == 1) ){

    case <- rep(case, n_it)

  } else {

    check_cross(ref_arg = col_name, eval_arg = case,
                arg_names = c('col_name', 'case') )

  }


  #*+++++++++++++++
  #* function
  #*+++++++++++++++
  # get matrix
  mat_target <-
    x[ , col_name, drop = FALSE] %>%
    as.matrix()

  # get final matrix
  mat_swe <- mat_target

  for(i in 1:n_it){

    if(case[i] == 'sf'){
      # to snowfall
      aux <- diff( mat_swe[ , i] )
      aux <- ifelse(aux > 0 , aux, 0)

      mat_swe[ , i] <- c(0, aux)

      rm(aux)

    } else if(case[i] == 'm'){
      # to melt
      aux <- diff( mat_swe[ , i] )
      aux <- ifelse(aux < 0 , aux, 0)

      mat_swe[ , i] <- c(0, abs(aux) )

      rm(aux)

    }


  } # end for i loop



  # set out name
  if( !is.null(out_name) ){
    #* use out_name
    colnames(mat_swe) <- out_name

    df_out <- x %>% as.data.frame() # avoid tibble issues
    df_out[ , out_name] <- mat_swe

  } else {
    #* overwrite existing data frame

    df_out <- x %>% as.data.frame() # avoid tibble issues

    df_out[ , col_name] <- mat_swe

  }

  # matrix issue
  df_out[ , -1] <- as.matrix( df_out[ , -1] )


  #* return
  return(df_out %>% as_tibble())


}
