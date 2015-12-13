// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcpp_add_biases
NumericMatrix rcpp_add_biases(NumericMatrix m, NumericVector v);
RcppExport SEXP mistnet2_rcpp_add_biases(SEXP mSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    __result = Rcpp::wrap(rcpp_add_biases(m, v));
    return __result;
END_RCPP
}
// ifelse_matrix_cpp
NumericMatrix ifelse_matrix_cpp(LogicalMatrix test, NumericMatrix yes, NumericMatrix no);
RcppExport SEXP mistnet2_ifelse_matrix_cpp(SEXP testSEXP, SEXP yesSEXP, SEXP noSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< LogicalMatrix >::type test(testSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type yes(yesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type no(noSEXP);
    __result = Rcpp::wrap(ifelse_matrix_cpp(test, yes, no));
    return __result;
END_RCPP
}
// matrixMultiplyGrad
NumericMatrix matrixMultiplyGrad(int n_out, NumericMatrix error_grad, NumericMatrix input_act);
RcppExport SEXP mistnet2_matrixMultiplyGrad(SEXP n_outSEXP, SEXP error_gradSEXP, SEXP input_actSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type n_out(n_outSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type error_grad(error_gradSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type input_act(input_actSEXP);
    __result = Rcpp::wrap(matrixMultiplyGrad(n_out, error_grad, input_act));
    return __result;
END_RCPP
}