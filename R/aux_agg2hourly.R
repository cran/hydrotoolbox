#' Aggregates a data frame to hourly resolution
#'
#' @param df data frame with class Date or POSIXct in the first column.
#' @param col_name string (vector) with the name(s) of the column(s) to aggregate.
#' @param fun string (vector) containing the name(s) of function(s) to use: mean, min, max, sum, last or first.
#' @param allow_na optional. Numeric value with the maximum allowed number of NA_real_ values. By default the function will not tolerate any NA_real_ in an aggregation period (and will return NA_real_ instead).
#'
#' @return A data frame with the Date and the aggregated variable(s).
#'
#' @export
#'
#' @keywords internal
#'
agg2hourly <- function(df, col_name, fun, allow_na = 0){
  # set date format
  date_format <- '%Y-%m-%d %H'

  # extract date series with the new format
  date_agg    <- format(df[ , 1], format = date_format)

  # convert to unique
  date_unique <- unique(date_agg) # sort()?

  # creates the output POSIXct vector
  date_out    <- as.POSIXct( paste0(date_unique, ':00:00'), tz = 'UTM' )

  # matrix creation for output and iteration
  n_it  <- length(date_out) # number of iterations
  n_col <- length(col_name) # output matrix columns

  mat_in  <- as.matrix( df[ , col_name, drop = FALSE] )
  mat_out <- matrix(NA_real_, nrow = n_it, ncol = n_col)

  # get grouping column number of stats
  fun_string <- c('sum', 'max', 'min', 'mean', 'first', 'last') # keep in mind this order
  col_group  <- match(x = fun, table = fun_string)

  group_unique <- unique(col_group) # to get the fun_string to use
  n_group      <- length(group_unique) # to loop by unique fun_string

  # loop to get stats
  for(i in 1:n_it){
    # get row index
    row_index <- which(date_agg == date_unique[i])

    # get the statistic
    for(j in 1:n_group){
      col_index  <- which(fun == fun_string[ group_unique[j] ])
      sub_matrix <- mat_in[row_index, col_index, drop = FALSE]

      mat_out[i, col_index] <- col_stats_cpp(x = sub_matrix, stats = fun_string[ group_unique[j] ], allow_na = allow_na)

      rm(col_index, sub_matrix, j)

    } # end for j loop


    rm(i, row_index)

  } # end for i loop


  return( data.frame(Date = date_out, mat_out) )


} # end function
