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
# Center aligned moving average windows
#
# @description Smooth numeric series with a center aligned moving average windows

mov_center <- function(x, ws){
  n_it   <- nrow(x)
  moduli <- ws %% 2
  out    <- matrix( NA_real_, nrow = n_it, ncol = ncol(x) )

  if( moduli == 1 ){
    ## uneven case
    w_left <- w_right <- trunc( ws / 2 ); # to get the left and right limits of the windows

    ## loop
    for(i in 1:n_it){
      # get first and last rows index
      ind_first <- max(i - w_left, 1);
      ind_last  <- min(i + w_right, n_it);

      # subset matrix
      x_sub <- x[ind_first:ind_last, , drop = FALSE];

      out[i, ] <- col_mean(x = x_sub, allow_na = ws);

    } # end for loop

    return(out);


  } else {
    # even case
    w_left  <- ws / 2 - 1;
    w_right <- ws / 2;

    # loop
    ## loop
    for(i in 1:n_it){
      # get first and last rows index
      ind_first <- max(i - w_left, 1);
      ind_last  <- min(i + w_right, n_it);

      # subset matrix
      x_sub <- x[ind_first:ind_last, , drop = FALSE];

      out[i, ] <- col_mean(x = x_sub, allow_na = ws);

    } # end for loop

    return(out);

}




}


