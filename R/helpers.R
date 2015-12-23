# Functions used internally that don't need to be made visible
# to the user


# Add each element of `vector` to the corresponding column, instead
# of adding the whole vector to each column as the recycling rule would do
`%plus%` = function(matrix, vector){
  rcpp_add_biases(matrix, vector)
}

# several times faster than R's built-in `ifelse` function; useful for
# activation functions that are called many times
ifelse_matrix = function(test, yes, no){

  # Without this assertion, any nonzero entries would be coerced to TRUE
  if (!is.logical(test)) {
    stop("test is not logical")
  }

  ifelse_matrix_cpp(test, yes, no)
}
