#' Creates suitable table for \code{ggplot2}
#'
#' @param df data frame with class Date or POSIXct in the first column and the variables to plot on the others.
#' @param l_color string vector with a color for each line (data frame column).
#' @param l_type string vector with the line type.
#' @param l_size numeric vector with the line size.
#' @param l_legend string vector with the user name for legend labels.
#'
#' @return A data frame ready to use in \code{ggplot2}.
#'
#' @importFrom reshape2 melt
#'
#' @export
#'
#' @keywords internal
#'
ggplot_table <- function(df, l_color, l_type, l_size, l_legend = NULL){

  # get row number in order to generate lines features
  n_rep <- nrow(df)

  # convert the data frame
  gg_table <- melt(data = df, id.vars = 'date',
                   variable.name = 'group',
                   value.name = 'series')

  # add lines features
  gg_table[ , 'l_color'] <- c(
    sapply(X = l_color,
           FUN = function(x){ rep(x, n_rep) } )
    )

  gg_table[ , 'l_type'] <- c(
    sapply(X = l_type,
           FUN = function(x){ rep(x, n_rep) } )
  )


  gg_table[ , 'l_size'] <- c(
    sapply(X = l_size,
           FUN = function(x){ rep(x, n_rep) } )
  )

  if( is.null(l_legend) == FALSE ){
    gg_table[ , 'group'] <- factor(
      c(
      sapply(X = l_legend,
             FUN = function(x){ rep(x, n_rep) } )
    ),
    levels = l_legend )

  }

  # return table
  return(gg_table)
}
