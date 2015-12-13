`%plus%` = function(matrix, vector){
  rcpp_add_biases(matrix, vector)
}

#' @import assertthat
ifelse_matrix = function(test, yes, no){

  # Without this assertion, any nonzero entries would be coerced to TRUE
  assert_that(is.logical(test))

  ifelse_matrix_cpp(test, yes, no)
}
