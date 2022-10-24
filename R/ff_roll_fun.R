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
#' Rolling functions
#'
#' @description It provides a generic function to rolling table columns.
#' Internally it is using \code{rollapplyr} from package \code{zoo}.
#'
#' @param x data frame (or tibble) with class \code{Date} or
#' \code{POSIX*} in the first column.
#' @param col_name string vector with the column(s) name(s) of the series to roll.
#' The default value uses the \code{'last'} column. Another single string choice
#' is to use \code{'all'}. Is important to keep in mind that this argument
#' \bold{commands}, so if you provide two columns
#' names, \code{k} and \code{pos} arguments must be of length two; if not the
#' single value will be recycled.
#' @param k numeric vector with the windows size. E.g.: \code{k = 5}.
#' @param pos string vector with the position of the windows: \itemize{
#'      \item 'c': center (default). The output value is in the middle of the window.
#'      \item 'l': left aligned. The output value is on the left, so the function weights
#'      the \code{(k - 1)} values at the right side.
#'      \item 'r': right aligned. The output value is on the right, so the function weights
#'      the \code{(k - 1)} values at the left side.
#' }
#' @param FUN the function to be applied.
#' @param ... optional arguments to \code{FUN}.
#' @param out_name optional. String vector with new column names. If you set
#' it as \code{NULL} the function will overwrite the original series.
#' @param from optional. String value for \code{'Date'} class or \code{POSIX*}
#' class for date-time data containing the starting \code{Date}.
#' @param to optional. String value for \code{'Date'} class or \code{POSIX*}
#' class for date-time data containing the ending \code{Date}.
#'
#' @return The same table but with the rolling series.
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#'
#' @examples
#'
#' # read guido daily streamflow records
#' path <- system.file('extdata', 'snih_qd_guido.xlsx',
#'         package = 'hydrotoolbox')
#'
#' # read and apply the function
#' qd_guido <-
#'   read_snih(path = path, by = 'day', out_name = 'q(m3/s)') %>%
#'   roll_fun(k = 5, FUN = mean, na.rm = TRUE,
#'    out_name = 'q_smooth')
#'
#'
#'
roll_fun <- function(x,
                     col_name = 'last',
                     k,
                     pos = 'c',
                     FUN,
                     ...,
                     out_name = NULL,
                     from = NULL,
                     to = NULL){
  #*+++++++++++++++++
  #* conditionals
  #*+++++++++++++++++
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

  n_cn <- length(col_name)
  if( n_cn  == 1 ){

    check_string(argument = col_name,
                 target = c( 'last',
                             'all', colnames(x)[-1] ),
                 arg_name = 'col_name')

    # set col_name values
    if(col_name == 'last'){

      col_name <- colnames(x)[ ncol(x) ]

    } else if(col_name == 'all'){

      col_name <- colnames(x)[-1]

    }



  } else{

    check_string(argument = col_name,
                 target = colnames(x)[-1],
                 arg_name = 'col_name')

  }

  #* k
  check_class(argument = k,
              target = 'numeric',
              arg_name = 'k')
  if( length(k) == 1){
    # to recycle
    k <- rep(k, n_cn)

  } else {

    check_cross(ref_arg = col_name,
                eval_arg = k,
                arg_names = c('col_name', 'k') )

  }

  #* pos
  check_class(argument = pos,
              target = 'character',
              arg_name = 'pos')
  check_string(argument = pos,
               target = c('c', 'l', 'r'),
               arg_name = 'pos')
  if( length(pos) == 1){
    # to recycle
    pos <- rep(pos, n_cn)

  } else {

    check_cross(ref_arg = col_name,
                eval_arg = pos,
                arg_names = c('col_name', 'pos') )

  }

  #* FUN
  check_class(argument = FUN,
              target = 'function',
              arg_name = 'FUN')

  # adecuate to rollapplyr
  pos <-
    pos %>%
    gsub(pattern = "^l", replacement = "left") %>%
    gsub(pattern = "^c", replacement = "center") %>%
    gsub(pattern = "^r", replacement = "right")


  #* out_name
  if( !is.null(out_name) ){
    check_class(argument = out_name,
                target = 'character',
                arg_name = 'out_name')

    check_cross(ref_arg = col_name,
                eval_arg = out_name,
                arg_names = c('col_name', 'out_name') )

    # check for non-repeated names
    guess <- which(
      match(x = out_name, table = colnames(x) ) >= 1
    )
    is_na <- length(guess)

    if( is_na != 0 ){
      error_message <-
        paste0('Please check for ', out_name,
               ' argument. The following strings are forbidden: ',
               paste( colnames(x) , collapse = ', '), '.')

      stop( error_message, call. = FALSE )
    }


  }

  #* from and to
  if( !is.null(from) & !is.null(to) ){

    check_class(argument = from,
                target = c("Date", "POSIXct", "POSIXlt"),
                arg_name = 'from')

    check_class(argument = to,
                target = c("Date", "POSIXct", "POSIXlt"),
                arg_name = 'to')

    check_length(argument = from, max_allow = 1, arg_name = 'from')
    check_length(argument = to, max_allow = 1, arg_name = 'to')

    check_cross(ref_arg = from, eval_arg = to, arg_names = c('from', 'to') )


  }

  #*++++++++++++++
  #* function
  #*++++++++++++++
  #* get index
  if( !is.null(from) & !is.null(to) ){

    ind_first <- which( x[ , 1, drop = TRUE] == from )
    ind_last  <- which( x[ , 1, drop = TRUE] == to )

  } else {

    ind_first <- 1
    ind_last  <- nrow(x)

  }

  #* subset data frame
  x_sub <- subset(x = x[ind_first:ind_last, ],
                  select = col_name)

  #* get matrix
  x_mat <- as.matrix( x_sub ) # because in the previous line I drop date column

  # check for numeric class
  check_class(argument = c(x_mat),
              target = "numeric",
              arg_name = "col_name values")

  #* moving average
  x_smooth <- matrix(data = NA_real_, nrow = nrow(x_mat), ncol = n_cn )

  for(j in 1:n_cn){
    # because col_name commands

    x_pass   <- x_mat[ , col_name[j], drop = FALSE ]

    x_mov <-
      x_pass %>%
      rollapplyr(width = k[j],
                 align = pos[j],
                 FUN,
                 ...,
                 fill = list(NA_real_)
      )


    x_smooth[ , j] <- x_mov

  }


  #* out_name
  if( !is.null(out_name) ){
    #* TRUE  => set new col names and merge
    #* build data frame
    df <- data.frame( x[ind_first:ind_last , 'date'], x_smooth )
    colnames(df) <- c('date', out_name)

    out <- merge(x = x, y = df, all.x = TRUE)

    # set original values to NA_values in smooth series
    non_ind <- -(ind_first:ind_last)
    for(i in 1:n_cn){

      out[ non_ind, out_name[i] ] <- out[ non_ind, col_name[i] ]

    }

  } else{
    #* FALSE => set smooth rows in columns
    #* just set smooth values
    x[ind_first:ind_last, col_name] <- x_smooth

    out <- x

  }

  #* return
  return(out %>% as_tibble())


}
