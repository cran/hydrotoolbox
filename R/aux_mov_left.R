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
# Left aligned moving average windows
#
# @description Smooth numeric series with a left aligned moving average windows

mov_left <- function(x, ws){
  n_it   <- nrow(x)
  out    <- matrix( NA_real_, nrow = n_it, ncol = ncol(x) )

  w_left  <- 0
  w_right <- ws - 1
  ## loop
  for(i in 1:n_it){
      # get first and last rows index
      ind_first <- max(i - w_left, 1);
      ind_last  <- min(i + w_right, n_it);

      # subset matrix
      x_sub <- x[ind_first:ind_last, , drop = FALSE];

      out[i, ] <- col_mean(x = x_sub, allow_na = ws);

    } # end for loop

  return(out)

}
