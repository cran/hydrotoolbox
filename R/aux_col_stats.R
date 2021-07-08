#' Get an specific matrix columns statistics.
#'
#' @param x numeric matrix
#' @param stats string vector with the statistics to compute (sum, max, min, mean, first or last)
#' @param allow_na number of allowed NA_real_
#'
#' @return Numeric vector with one value per matrix column
#'
#' @export
#'
#' @keywords internal
#'
col_stats <- function(x, stats, allow_na){

  if(stats == 'sum'){
    out <- col_sum(x = x, allow_na = allow_na)

  } else if( stats == 'max'){
    out <- col_max(x = x, allow_na = allow_na)

  } else if( stats == 'min'){
    out <- col_min(x = x, allow_na = allow_na)

  } else if( stats == 'mean'){
    out <- col_mean(x = x, allow_na = allow_na)

  } else if(stats == 'first'){
    out <- col_first(x = x, allow_na = allow_na)

  } else if(stats == 'last'){
    out <- col_last(x = x, allow_na = allow_na)

  }

  return(out)


} # end function
