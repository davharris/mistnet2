#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix ifelse_matrix(LogicalMatrix m, NumericMatrix yes, NumericMatrix no){
  NumericMatrix out(m.nrow(), m.ncol());

  for(int i = 0; i< m.ncol(); i++){
    out(_, i) = ifelse(
      m(_, i),
      yes(_, i),
      no(_, i)
    );
  };

  return out;
}
