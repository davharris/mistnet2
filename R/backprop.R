#' Backprop: calculate network gradients using backpropagation
#' @param network a \code{network} object, as created by \code{\link{mistnet}}
#' @param state if a \code{network_sate} object is provided, it can be used
#'    instead of producing a new one with \code{\link{feedforward}}. This may
#'    save some computation time.
#' @param ... (currently not used)
#' @return a \code{vector} of gradients.
#' @export
backprop = function(network, state, ...){
  # We'll be filling in lists of gradients for the weights and biases.
  # Start them with empty lists.  Gradient for z is defined later.
  weight_grads = bias_grads = list()

  if(missing(state)){
    state = feedforward(network)
  }

  # Start from the top of the network and work downwards:
  for(i in length(network$weights):1){

    # Gradient of the activation function (nonlinearity) for this layer
    activation_grad = network$activators[[i]]$grad(
      network$pre_activations[[i]]
    )

    # The weights and biases depend on
    grad_from_above = if(i == length(network$weights)){
      # The top layer's job is to follow the gradient with respect to the
      # outputs, as determined by the error function (eg Gaussian or binomial
      # errors)
      network$error$grad(state$outputs[[i]]) * activation_grad
    }else{
      # Lower layers' jobs are to follow the gradient from the inputs in the
      # layer above.
      input_grad * activation_grad
    }

    weight_grads[[i]] = crossprod(cbind(network$x, network$z), grad_from_above)
    bias_grads[[i]] = colSums(grad_from_above)

    # Matrix multiply the gradient by the weights to find gradient with respect
    # to the layer's inputs.  For layer 1, this will be used immediately below.
    # For higher layers, it won't be used until stepping through the loop again.
    input_grad = tcrossprod(grad_from_above, network$weights[[i]])

    if(i == 1){
      # The z_gradients are just the input gradients for the non-x columns
      z_grads = input_grad[ , -(1:ncol(network$x))]
    }
  }

  list(weight_grads, bias_grads, z_grads)
}
