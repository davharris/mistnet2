predict.network = function(object, newdata, ...){
  object$x = newdata
  feedforward(object)
}

predict.mistnet_network = function(
  object,
  newdata,
  n_samples,
  return_all = TRUE,
  ...
){
  object$x = newdata

  # Draw n_samples sample matrices from z's prior distribution
  z_samples = replicate(
    n_samples,
    {
      matrix(
        draw_samples(object$z_prior, n = nrow(newdata) * object$n_z),
        nrow = nrow(new_data),
        ncol = n_z
      )
    },
    simplify = FALSE
  )

  # Calculate network state for each sample
  states = feedforward_with_samples(object, z_samples)

  # return everything, or just an array of network outputs?
  if (return_all) {
    out = states
  } else {
    out = array(NA, dim = c(n_x, ncol(object$y), n_samples))
    n_layers = length(object$activations)
    for (i in 1:n_samples) {
      out[ , , i] = states[[i]]$output[[n_layers]]
    }
  }

  out
}
