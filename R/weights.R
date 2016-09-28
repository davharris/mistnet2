make_weight_list = function(n_x, n_z, layers) {
  n_layers = length(layers)

  weight_dims = numeric(n_layers + 1)

  weight_dims[1] = n_x + n_z  # nrow of weight matrix 1

  for (i in 1:n_layers) {
    # ncol of weight matrix i
    weight_dims[i + 1] = layers[[i]]$n_nodes
  }

  lapply(
    1:n_layers,
    function(i){
      initialize_weights(
        layers[[i]],
        n_in = weight_dims[i],
        n_out = weight_dims[i + 1]
      )
    }
  )
}


initialize_weights = function(layer, n_in, n_out,
                              initializer = initialize_weights_glorot_normal){

  if (is.null(layer$weights)) {
    weights = initializer(n_in = n_in, n_out = n_out)
  } else{
    assert_that(are_equal(dim(layer$weights), c(n_in, n_out)))
    weights = layer$weights
  }
  weights
}

#' @importFrom stats rnorm
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
