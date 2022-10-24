#' Aggregates a data frame to annually resolution
#'
#' @param df data frame with class Date or POSIXct in the first column.
#' @param col_name string (vector) with the name(s) of the column(s) to aggregate.
#' @param fun string (vector) containing the name(s) of function(s) to use: mean, min, max, sum, last or first.
#' @param allow_na optional. Numeric value with the maximum allowed number of NA_real_ values. By default the function will not tolerate any NA_real_ in an aggregation period (and will return NA_real_ instead).
#' @param start_month numeric value defining the first month of the annually period. Default sets to 1 (January).
#' @param end_month numeric value defining the last month of the annually period. Default sets to 12 (December).
#'
#' @return A data frame with the Date and the aggregated variable(s).
#'
#' @export
#' @keywords internal
#'
agg2annually <- function(df,
                         col_name,
                         fun,
                         allow_na = 0,
                         start_month = 1,
                         end_month = 12){
  # set date format
  date_format <- '%Y-%m'

  # extract date series with the new format
  date_agg    <- format(df[ , 1, drop = TRUE], format = date_format)
  year_agg    <- format(df[ , 1, drop = TRUE], format = '%Y')

  # convert to unique
  date_unique <- unique(date_agg) # sort()?

  # create vector with starting and ending dates
  y_first <- as.numeric( year_agg[1] )
  y_last  <- as.numeric( year_agg[ length(year_agg) ] )

    # check date existence
  month_first <- ifelse(start_month < 10, paste0('0', start_month), start_month ) # just format
  month_last  <- ifelse(end_month < 10, paste0('0', end_month), end_month ) # just format

  target_first <- paste0(y_first, '-', month_first)
  target_last  <- paste0(y_last, '-', month_last)

  exist_first <- which(date_unique == target_first)
  exist_last  <- which(date_unique == target_last)

    # sum or rest a year to get date consistency
  if(length(exist_first) == 0){
    target_first <- paste0( (y_first + 1), '-', month_first )
  }

  if(length(exist_last) == 0){
    target_last <- paste0( (y_last - 1), '-', month_last )
  }

  date_first <- as.Date( paste0(target_first, '-01'), format = '%Y-%m-%d' )
  date_last  <- as.Date( paste0(target_last, '-01'), format = '%Y-%m-%d' )

    # pick the first and last dates
  begin_first <- date_first
  end_last    <- date_last

    # to get corrected years
  n_y_first <- as.numeric( format(begin_first, format = '%Y') )
  n_y_last  <- as.numeric( format(end_last, format = '%Y') )

  if(start_month <= end_month){
    begin_last <- as.Date( paste0( n_y_last, '-', month_first, '-01' )  )
    end_first  <- as.Date( paste0( n_y_first, '-', month_last, '-01' )  )

  } else {
    begin_last <- as.Date( paste0( (n_y_last - 1), '-', month_first, '-01' )  )
    end_first  <- as.Date( paste0( (n_y_first + 1), '-', month_last, '-01' )  )

  }


    # make the date.sequence
  begin_vect <- seq.Date(from = begin_first, to = begin_last, by = 'year')
  end_vect   <- seq.Date(from = end_first, to = end_last, by = 'year')

    # flag vectors to compare and get rows in for loop
  flag_begin <- format(begin_vect, '%Y-%m')
  flag_end   <- format(end_vect, '%Y-%m')


  # matrix creation for output and iteration
  n_it  <- length(begin_vect) # number of iterations
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
    row_index <- which(date_agg >= flag_begin[i] & date_agg <= flag_end[i])

    # get the statistic
    for(j in 1:n_group){
      col_index  <- which(fun == fun_string[ group_unique[j] ])
      sub_matrix <- mat_in[row_index, col_index, drop = FALSE]

      mat_out[i, col_index] <- col_stats_cpp(x = sub_matrix, stats = fun_string[ group_unique[j] ], allow_na = allow_na)

      rm(col_index, sub_matrix, j)

    } # end for j loop


    rm(i, row_index)

  } # end for i loop


  return( data.frame(begin = begin_vect, end = end_vect, mat_out) %>%
            as_tibble()
          )


} # end function
