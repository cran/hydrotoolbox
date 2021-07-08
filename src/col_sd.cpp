#include <Rcpp.h>
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

// [[Rcpp::export]]
NumericVector col_sd(NumericMatrix x, int allow_na){
  // variable declaration
  double n_col = x.ncol();
  int n_na;
  LogicalVector l_na;
  NumericVector out(n_col);

  for(int i = 0; i < n_col; ++i){

    NumericMatrix::Column col_target = x( _ , i);

    // get number of na_real
    l_na = is_na( col_target );
    n_na = sum(l_na);

    // compare with allow_na
    if(n_na > allow_na){
      // set output as NA_REAL_
      out(i) = NA_REAL;

    } else {
      // remove NA_REAL_ and get stats
      NumericVector aux = na_omit( col_target );
      out(i) = sd(aux);
    }



  }

  return(out);


}
