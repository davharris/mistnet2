#' @export
feedforward = function(network, ...){
  UseMethod("feedforward")
}

#' Feed forward: calculate network state from its coefficients
#' @param network a \code{network} object, as created by \code{\link{mistnet}}
#' @param par A list or vector containing the parameters. If not included,
#'    \code{network$par_list} will be used
#' @param ... (currently not used)
#' @return a \code{network_state} object, i.e. a \code{list} of \code{list}s.
#' \itemize{
#'    \item{\code{inputs}: A list of input matrices for each layer.  The first layer's input
#'        is concatenated from the network's \code{x} and \code{z} matrices using
#'        \code{cbind}, while subsequent inputs come from the previous
#'        layer's \code{output}}.
#'    \item{\code{pre_activations}: A list of pre_activation matrices for each layer,
#'        equal to the matrix product of the layer's \code{input} and its
#'        \code{weights}, plus its \code{biases}}.
#'    \item{\code{outputs}: A list of outputs for each layer, produced by applying the layer's
#'        activation function to the \code{pre_activations}}.
#' }
#' @aliases feedforward
#' @export
feedforward.network = function(network, par, ...){

  if (missing(par)) {
    parameters = network$par_list
  } else {
    parameters = build_par_list(par = par, par_list = network$par_list)
  }

  n_layers = length(parameters$weights)

  # inputs, pre-activations, and outputs start as lists of NULL
  empty_list = replicate(n_layers, NULL, simplify = FALSE)
  inputs = pre_activations = outputs = empty_list

  # Set up the first layer's inputs
  if (is(network, "mistnet_network")) {
    # Mistnet networks' first input layer concatenates x and z
    inputs[[1]] = cbind(network$x, parameters$z)
  } else {
    # Networks with fully-observed inputs just take x
    inputs[[1]] = network$x
  }

  # Fill in the inputs, pre_activations, and outputs
  for (i in 1:n_layers) {
    if (is.null(inputs[[i]])) {
      inputs[[i]] = outputs[[i - 1]]
    }

    # Matrix multiply inputs by weights and add biases
    pre_activations[[i]] = inputs[[i]] %*% parameters$weights[[i]] %plus%
      parameters$biases[[i]]

    # Apply the activation function
    outputs[[i]] = network$activators[[i]]$f(pre_activations[[i]])
  }

  dimnames(outputs[[n_layers]]) = dimnames(network$y)

  structure(
    list(
      inputs = inputs,
      pre_activations = pre_activations,
      outputs = outputs
    ),
    class = "network_state"
  )
}


