#' Backprop: calculate network gradients using backpropagation
#' @param network a \code{network} object, as created by \code{\link{mistnet}}
#' @param state if a \code{network_sate} object is provided, it can be used
#'    instead of producing a new one with \code{\link{feedforward}}. This may
#'    save some computation time.
#' @param par A vector containing the parameters
#' @param ... (currently not used)
#' @return a \code{vector} of gradients.
#' @export
backprop = function(network, state, par, ...){

  parameters = relist(par, network$par_list)

  # We'll be filling in lists of gradients for the weights and biases.
  # Start them with empty lists.  Gradient for z is defined later.
  weight_grads = bias_grads = list()

  if (missing(state)) {
    state = feedforward(network, par = par)
  }

  # Start from the top of the network and work downwards:
  for (i in length(parameters$weights):1) {

    # Gradient of the activation function (nonlinearity) for this layer
    activation_grad = network$activators[[i]]$grad(
      state$pre_activations[[i]]
    )

    # The weights and biases depend on gradients passed from higher levels in
    # the network.
    grad_from_above = if (i == length(parameters$weights)) {
      # The top layer's job is to follow the gradient with respect to the
      # outputs, as determined by the error function (eg Gaussian or binomial
      # errors)

      # So it takes the raw error gradient and multiplies it by activation grad
      # (because of the chain rule)
      grad(network$error_distribution,
           "mu",
           y = network$y,
           mu = state$outputs[[i]]) * activation_grad
    }else{
      # Lower layers' jobs are to follow the gradient from the inputs in the
      # layer above.  They also multiply by activation_grad because of the chain
      # rule. This code isn't run the first time through the loop, so the fact
      # that input_grad hasn't been defined by this line of the source is okay.
      input_grad * activation_grad
    }

    # Weight gradients depend on the input values and gradients from above
    weight_grads[[i]] = crossprod(state$inputs[[i]], grad_from_above) +
      grad(network$weight_priors[[i]], "x", x = parameters$weights[[i]])

    # Bias gradients just sum up the gradients (equivalent to matrix multiplying
    # by a vector of ones)
    bias_grads[[i]] = colSums(grad_from_above)

    # Matrix multiply the gradient by the weights to find gradient with respect
    # to the layer's inputs.  For layer 1 (the last time through this loop),
    # input_grad will be used immediately. For higher layers, it won't be used
    # until stepping through the loop again.
    input_grad = tcrossprod(grad_from_above, parameters$weights[[i]])

    if (i == 1) {
      # The z_gradients are just the input gradients for the non-x columns
      z_grads = input_grad[ , -(1:ncol(network$x))]
    }
  }

  out = list(z = z_grads, weights = weight_grads, biases = bias_grads)

  # Force the output to have the same ordering as the original parameters so
  # that unlisting/relisting doesn't destroy information
  out[names(parameters)]
}
