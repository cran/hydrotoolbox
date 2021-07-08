#include <Rcpp.h>
#include "col_first.h"
#include "col_last.h"
#include "col_max.h"
#include "col_min.h"
#include "col_mean.h"
#include "col_sum.h"
using namespace Rcpp;

// **********************************************************
//  Author       : Ezequiel Toum
//  Licence      : GPL V3
//  Institution  : IANIGLA-CONICET
//  e-mail       : etoum@mendoza-conicet.gob.ar
//  **********************************************************
//  hydrotoolbox package is distributed in the hope that it
//  will be useful but WITHOUT ANY WARRANTY.
//  **********************************************************

// @param x numeric matrix
// @param stats string with the statistics to compute (sum, max, min, mean, first or last)
// @param allow_na number of allowed NA_real_
//
// @return Numeric vector with one value per matrix column

// [[Rcpp::export]]
NumericVector col_stats_cpp(NumericMatrix x, std::string stats, double allow_na) {
  // variable declaration
  double n_col = x.ncol();
  NumericVector out(n_col);

  if(stats == "sum"){
    out = col_sum(x = x, allow_na = allow_na);

  } else if( stats == "max"){
    out = col_max(x = x, allow_na = allow_na);

  } else if( stats == "min"){
    out = col_min(x = x, allow_na = allow_na);

  } else if( stats == "mean"){
    out = col_mean(x = x, allow_na = allow_na);

  } else if(stats == "first"){
    out = col_first(x = x, allow_na = allow_na);

  } else if(stats == "last"){
    out = col_last(x = x, allow_na = allow_na);

  }

  return(out);


}
