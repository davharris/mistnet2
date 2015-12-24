#' Describe a layer of a neural network
#' @param activator an \code{\link{activator}} object, determining the nonlinear
#'    function performed by this layer and its derivatives
#' @param n_nodes a whole number determining the size of the layer. The network's
#'    last layer should have the same number of nodes as there are columns in
#'    \code{y}.
#' @param weight_prior a \code{\link{distribution}} object that acts as a prior
#'    distribution for the weight matrix.
#' @param ... Additional objects to include in the layer (not currently used
#'    by the \code{\link{mistnet}} function)
#' @param weights an optional weight matrix; if omitted, the
#'   \code{\link{mistnet}} function will randomly initialize the weights
#' @param biases an optional bias vector; if omitted, the
#'   \code{\link{mistnet}} function will randomly initialize the biases
#' @return a list of class \code{layer} containing the objects above
#' @export
layer = function(activator, n_nodes, weight_prior, ..., weights = NULL,
                 biases = NULL){

  stopifnot(length(n_nodes) == 1)
  if (n_nodes < 1 | !all.equal(n_nodes, round(n_nodes))) {
    stop("n_nodes must be a whole number")
  }
  if (!is.null(biases)) {
    stopifnot(length(biases) == n_nodes)
  }

  stopifnot(is(activator, "activator"))
  stopifnot(is(weight_prior, "distribution"))


  structure(
    list(activator = activator,
         n_nodes = n_nodes,
         weight_prior = weight_prior,
         ...,
         weights = weights,
         biases = biases
    ),
    class = "layer"
  )
}
