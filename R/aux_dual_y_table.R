#' Rescale the data frame values in order to graph a dual y axis plot
#'
#' @param df data frame provided by \code{build_table} function.
#' @param y_left string vector with column names of the variables to plot in the left y axis.
#' @param y_right string vector with column names of the variables to plot in the right y axis.
#'
#' @return A list containing the rescaled data frame and the transformation formula.
#'
#' @export
#'
#' @keywords internal
#'
dual_y_table <- function(df, y_left, y_right){
  # source: http://rstudio-pubs-static.s3.amazonaws.com/329613_f53e84d1a18840d5a1df55efb90739d9.html

  #********************************************************************
  # set the vectors for the left and right y axis
  #********************************************************************
  y1 <- unlist(
    c( df[ , y_left] ), use.names = FALSE
  )  # left

  y2 <- unlist(
    c( df[ , y_right] ), use.names = FALSE
  )  # right

  df_out <- df


  #********************************************************************
  # Rescale the second y axis by
  #   - subtracting its minimum value (to set it to start at 0)
  #   - scaling so that it has the same range as the 'y1' variable
  #   - offsettting it by the minimum value of y1
  #********************************************************************
  a            <- range(y1, na.rm = TRUE)
  b            <- range(y2, na.rm = TRUE)
  scale_factor <- diff(a)/diff(b)

  # rescale values
  n_it <- length(y_right)
  for (i in 1:n_it) {
    df_out[[ y_right[i] ]] <- (df_out[[ y_right[i] ]] - b[1]) * scale_factor + a[1]
  }

  #********************************************************************
  # Need to define the second axis transformation to be the inverse of
  # the data transformation to everything cancels out appropriately
  #********************************************************************
  trans <- ~ ((. - a) / scale_factor) + b

  # build the output list
  out <- list(table = df_out,
              coefficients = c(a = a[1], b = b[1], sf = scale_factor),
              transformation = trans)

  return(out)
}
