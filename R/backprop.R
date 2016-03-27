#' @export
backprop = function(network, ...){
  UseMethod("backprop")
}

#' Backprop: calculate network gradients using backpropagation
#' @param network a \code{network} object, as created by \code{\link{mistnet}}
#' @param state if a \code{network_sate} object is provided, it can be used
#'    instead of producing a new one with \code{\link{feedforward}}. This may
#'    save some computation time.
#' @param par A list or vector containing the parameters. If not included,
#'    \code{network$par_list} will be used
#' @param ... (currently not used)
#' @return a \code{vector} of gradients.
#' @aliases backprop
#' @export
backprop.mistnet_network = function(network, state, par, ...){

  if (missing(par)) {
    parameters = network$par_list
  } else {
    parameters = build_par_list(par = par, par_list = network$par_list)
  }

  if (missing(state)) {
    state = feedforward(network, par = par)
  }

  # Start with the gradients for the error distribution itself
  error_distribution_grads = all_error_grads(
    error_distribution = network$error_distribution,
    error_distribution_par = parameters$error_distribution_par,
    y = network$y,
    mu = state$outputs[[length(state$outputs)]]
  )

  weight_grads = bias_grads = list()

  input_grad = error_distribution_grads$mu

  # Start from the top of the network and work down to layer 1:
  for (i in length(parameters$weights):1) {

    # Gradient of the activation function (nonlinearity) for this layer
    activation_grad = network$activators[[i]]$grad(
      state$pre_activations[[i]]
    )

    # Take the gradient from above and multiply by activation_grad because of
    # the chain rule.
    pre_activation_grad = input_grad * activation_grad

    # Weight gradients depend on the input values and gradients from above
    # (i.e. pre_activation_grad)
    weight_grads[[i]] = crossprod(state$inputs[[i]], pre_activation_grad) +
      grad(network$weight_priors[[i]], "x", x = parameters$weights[[i]])

    # Bias gradients just sum up the gradients (equivalent to matrix multiplying
    # by a vector of ones)
    bias_grads[[i]] = colSums(pre_activation_grad)

    # Matrix multiply the gradient by the weights to find gradient with respect
    # to the layer's inputs.
    input_grad = tcrossprod(pre_activation_grad, parameters$weights[[i]])

    if (i == 1) {
      # The z_gradients are just the input gradients for the non-x columns
      # plus the gradients of their prior; they only live in the first layer.
      z_grads = input_grad[ , -(1:ncol(network$x))] +
        grad(network$z_prior, "x", x = parameters$z)
    }
  }


  par_names = names(parameters$error_distribution_par)
  out = list(z = z_grads, weights = weight_grads, biases = bias_grads,
             error_distribution_par = error_distribution_grads[par_names])

  # Force the output to have the same ordering as the original parameters so
  # that unlisting/relisting doesn't destroy information
  out[names(parameters)]
}
