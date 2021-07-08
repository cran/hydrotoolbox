#' Create a basic data frame for plotting
#'
#' @param hm_obj a valid \code{hydromet_xxx} class object.
#' @param slot_name string vector with the slot(s) to extract from the \code{hm_obj}.
#' @param col_name a list containing a vector string per slot with the name(s) of the column(s) to plot.
#' @param from string (or \code{POSIXct}) with the starting \code{date}. You can use 'from' without 'to'. In this case you will subset your data 'from' till the end.
#' @param to string (or \code{POSIXct}) with the ending \code{date}. You can use 'to' without 'from'. In this case you will subset your data from the beginning till 'to'.
#'
#' @return A data frame with the date in the first column and the variables to plot on the others.
#'
#'
#' @export
#'
#' @keywords internal
#'
build_table <- function(hm_obj, slot_name, col_name, from = NULL, to = NULL){

  # loop in the slots
  n_slot <- length(slot_name)

  for (i in 1:n_slot) {

    if(i == 1){
      final_table <-
        hm_get(obj = hm_obj, slot_name = slot_name[i]) %>%
        subset(select = c( 'date', col_name[[i]] ) )

    } else{
      next_table <-
        hm_get(obj = hm_obj, slot_name = slot_name[i]) %>%
        subset(select = c( 'date', col_name[[i]] ) )

      final_table <- merge(x = final_table, y = next_table, all.x = TRUE)

      rm(next_table)

    }



  }

  # subset by date
  date <- NULL # to avoid compilation issues

  if( is.null(from) == FALSE & is.null(to) == FALSE ){
    out_table <- subset(x = final_table,
                        subset = date >= from & date <= to)

  } else if( is.null(from) == FALSE & is.null(to) == TRUE ){
    out_table <- subset(x = final_table,
                        subset = date >= from)

  } else if( is.null(from) == TRUE & is.null(to) == FALSE ){
    out_table <- subset(x = final_table,
                        subset = date <= to)

  } else{
    out_table <- final_table
  }

  # return table
  return(out_table)

}
