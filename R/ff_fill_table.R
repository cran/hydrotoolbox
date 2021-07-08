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
#' Find non-reported dates and fill them with \code{NA_real_}
#'
#' @description Automatically finds non recorded date periods and fills them with \code{NA_real_} values.
#'
#' @param x data frame with class \code{Date} or \code{POSIXct} in the first column.
#' @param col_name string with column(s) name(s) to fill.
#' @param by string with a valid time step (e.g.: \code{'month', 'day', '6 hour', '3 hour',
#' '1 hour', '15 min' }).
#'
#' @return A data frame with the date and the filled numeric variable(s).
#'
#' @export
#'
#' @examples
#'
#' # let's use a synthetic example to illustrate the use of the function
#' dates <- seq.Date(from = as.Date('1980-01-01'),
#'                  to = as.Date('2020-01-01'), by = 'day' )
#' var   <- runif(n = length(dates), min = 0, max = 100)
#'
#' met_var <- data.frame(date = dates, random = var)[-c(50:100, 251, 38) , ]
#'
#' met_var_fill <- fill_table(x = met_var, by = 'day')
#'
#'
fill_table <- function(x, col_name = 'all', by = NULL){
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
  check_string(argument = col_name, target = c('all', colnames(x)[-1]), arg_name = 'col_name' )

    #* set first column as 'date'
  colnames(x) <- c('date', colnames(x)[-1] )

    #* arrange col_name arg if all
    if(col_name == 'all'){
      col_name <- colnames(x)

    } else{
      col_name <- c('date', col_name) # to include date column
    }

  #* by
  check_class(argument = by, target = 'character', arg_name = 'by')
  check_length(argument = by, max_allow = 1, arg_name = 'by')



  #**************************
  #* function
  #**************************
  #* subset x to the required col_name arg
  table_s <- subset(x = x, select = col_name )

  #* first and last date
  n_row <- nrow(table_s)

  time_min <- table_s[1, 1]
  time_max <- table_s[n_row, 1]

  #* create complete time sequence
  hour_flag <- length( grep(pattern = 'hour', x = by) )
  min_flag  <- length( grep(pattern = 'min', x = by) )

    # if necessary coerce as.Date
  if( hour_flag == 0 & min_flag == 0 ){

    all_dates <- seq(from = as.Date( time_min ),
                     to = as.Date( time_max ),
                     by = by)

  } else {

    all_dates <- seq(from = as.POSIXct( as.character(time_min), tz = 'UTC' ),
                     to = as.POSIXct( as.character(time_max), tz = 'UTC' ),
                     by = by)

  }

 #* merge data.frames
  table_out <-
    data.frame(date = all_dates) %>%
    merge(y = table_s, all.x = TRUE) %>%
    unique.data.frame()


  #* return table
  return(table_out)

}
