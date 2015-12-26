initialize_weights = function(layer, n_in, n_out){

  if (is.null(layer$weights)) {
      weights = initialize_weights_glorot_normal(n_in = n_in, n_out = n_out)
  } else{
    if (all(dim(layer$weights) == c(n_in, n_out))) {
      weights = layer$weights
    } else{
      stop("weight matrix has incorrect dimensions")
    }
  }

  weights
}

initialize_weights_glorot_normal = function(n_in, n_out){

  # Initizlization variance suggested by Xavier Glorot and Yoshua Bengio in
  # "Understanding the difficulty of training deep feedforward neural networks"
  # in the Journal of Machine Learning Research, 2010
  # See also http://andyljones.tumblr.com/post/110998971763/an-explanation-of-xavier-initialization
  glorot_variance = 2 / (n_in + n_out)

  matrix(
    rnorm(n_in * n_out, sd = sqrt(glorot_variance)),
    nrow = n_in,
    ncol = n_out
  )
}
