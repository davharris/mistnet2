#' Feed forward: calculate network state from its coefficients
#' @param network a \code{network} object, as created by \code{\link{mistnet}}
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
#' @export
feedforward = function(network, ...){
  inputs = pre_activations = outputs = list()

  for(i in 1:length(network$weights)){
    if(i == 1){
      # First layer's inputs are concatenated from x and z
      inputs[[i]] = cbind(network$x, network$z)
    }else{
      # Subsequent layers' inputs are given by the previous layers' outputs
      inputs[[i]] = outputs[[i - 1]]
    }

    pre_activations[[i]] = inputs[[i]] %*% network$weights[[i]] %plus%
      network$biases[[i]]
    outputs[[i]] = network$activators[[i]]$f(pre_activations)
  }

  structure(
    list(
      inputs = inputs,
      pre_activations = pre_activations,
      outputs = outputs
    ),
    class = "network_state"
  )
}
