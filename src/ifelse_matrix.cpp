#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix ifelse_matrix_cpp(
    LogicalMatrix test,
    NumericMatrix yes,
    NumericMatrix no
){
  NumericMatrix out(test.nrow(), test.ncol());

  for(int i = 0; i< test.ncol(); i++){
    out(_, i) = ifelse(
      test(_, i),
      yes(_, i),
      no(_, i)
    );
  };

  return out;
}
