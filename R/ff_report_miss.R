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
#' Report \code{NA_real_} values inside a table.
#'
#' @description Creates a data frame with reported dates and number of times-step of missing or
#' not recorded data.
#'
#' @param x data frame with hydro-meteo data. First column is date and the second the numeric
#' vector to be reported.
#' @param col_name string vector with the column(s) name(s) to report. By default the function
#' will report all numeric columns.
#'
#' @return A list containing a data frame (one per \code{col_name}) with three columns:
#' start-date, end-date and number of missing time steps.
#'
#' @export
#'
#' @examples
#'
#' # read guido daily streamflow records
#' path <- system.file('extdata', 'snih_qd_guido.xlsx',
#'         package = 'hydrotoolbox')
#'
#' # load raw data
#' qd_guido <-
#'   read_snih(path = path, by = 'day', out_name = 'q(m3/s)') %>%
#'   mov_avg(k = 5, out_name = 'q_smooth')
#'
#' # get the data report
#' qd_guido %>%
#'   report_miss()
#'
#'
#'
report_miss <- function(x, col_name = 'all'){
  #**************************
  #* conditionals
  #**************************
  #* x
  check_class(argument = x, target = 'data.frame', arg_name = 'x')
  check_class(argument = x[ , 1], target = c('Date', 'POSIXct'), arg_name = 'x[ , 1]')
  check_class(argument = c( as.matrix( x[ , -1] ) ),
              target = 'numeric', arg_name = 'x[ , -1]')

  #* col_name
  check_class(argument = col_name, target = 'character', arg_name = 'col_name')
  if(col_name[1] != 'all'){
    check_string(argument = col_name, target = colnames(x)[-1], arg_name = 'col_name')
  }

  check_length(argument = col_name,
               max_allow = length( colnames(x)[-1] ),
               arg_name = 'col_name')


  #**************************
  #* function
  #**************************
  #* get number of columns to report
  all_names <- colnames(x)[-1]
  if(col_name[1] == 'all'){
    col_name <- all_names
    }

  col_pos <- match(x = col_name, table = all_names) + 1 # because column 1 is date

  #* loop to get all the missing data
  n_it     <- length(col_pos)
  out_list <- list()
  for(i in 1:n_it){
    # get missing data position
    na_pos <- which( is.na( x[ , col_pos[i] ] ) )

    # contiguous miss data
    contiguous <- c( 1, diff(na_pos) )

    # start and end of every interval
    first_pos <- which(contiguous > 1)

    if( length(contiguous) == 1 ){
      start_pos <- na_pos[1]
      end_pos   <- na_pos[1]

    } else {
      start_pos <- c( na_pos[1], na_pos[first_pos] )
      end_pos   <- c( na_pos[first_pos - 1], na_pos[ length(na_pos) ] )
    }

    # first and last date
    first_date <- c( x[start_pos, 1], NA_character_ )
    last_date  <- c( x[end_pos, 1], NA_character_)

    # time interval
    delta_t <- end_pos - start_pos + 1
    delta_t <- c( delta_t, sum(delta_t) )

    # create report miss table and fill out_list
    out_table <- data.frame(first = first_date, last = last_date, time_steps = delta_t)

    out_list[[ col_name[i] ]] <- out_table

    # remove
    rm(na_pos, contiguous, first_pos, start_pos, end_pos,
       first_date, last_date, delta_t, out_table)

  }# end for loop

  #* return list
  return(out_list)

}
