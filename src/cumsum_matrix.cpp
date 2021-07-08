#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix cumsum_matrix(NumericMatrix x) {
  // set variables
  int n_it = x.nrow(), n_jt = x.ncol();
  NumericVector row_cum( n_jt ), row_x( n_jt ) ;
  LogicalVector l_na( n_jt );
  NumericMatrix out(n_it, n_jt);

  // initialize matrix and vector
  row_cum = rep(0.0, n_jt);
  std::fill( out.begin(), out.end(), NumericVector::get_na() );

  for(int i = 0; i < n_it; ++i){

    // extract the row
    row_x = x(i, _);

    // if i == 0 just pass
    if(i == 0){

      out(i, _) = row_x;

      l_na = is_na(row_x);
      for(int j = 0; j < n_jt; ++j){

        if(l_na[j] == 0){

          row_cum[j] = row_x[j];

        }


      }// end for j



    } else {

      // now we enter in the vector elements
      l_na = is_na(row_x);
      for(int j = 0; j < n_jt; ++j){

        if(l_na[j] == 0){

          row_cum[j] += row_x[j];
          out(i, j)   = row_cum[j];

        } else {

          out(i, j) = NA_REAL;

        }


      }// end for j



    }



  }// end for i


 return(out);

}



