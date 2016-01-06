make_bias_list = function(layers, error_distribution, activators, y) {

  n_layers = length(layers)

  lapply(
    1:n_layers,
    function(i){
      if (is.null(layers[[i]]$biases)) {
        if (i < n_layers) {
          rep(0, layers[[i]]$n_nodes)
        } else {
          # binomial denominator is needed for the sigmoid activator and
          # is ignored by all the other ones.
          bd = error_distribution$family_parameters$bd
          activators[[i]]$initialize_activator_biases(y, bd = bd)
        }
      } else{
        layers[[i]]$biases
      }
    }
  )
}
