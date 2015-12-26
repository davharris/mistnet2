#' Build and fit a neural network with random effects
#' @param x A numeric matrix of predictor variables
#' @param y A numeric or integer matrix of response variables
#' @param n_z, The number of latent random variables to include as predictors
#'    alongside x
#' @param layers A list of \code{\link{layer}} objects. Note that \code{n_nodes}
#'    in the final layer must match \code{ncol(y)}.
#' @param error_distribution An \code{\link{distribution}} object determining
#'    the error distribution for the response variables y.
#' @param z_prior A \code{\link{distribution}} object (standard Gaussian by
#'    default) determining the prior on the latent variables.
#' @param fit Logical. Should the model be fitted or should an untrained model
#'    be returned? Defaults to \code{TRUE}.
#' @param mistnet_optimizer passed to \code{\link{mistnet_fit}}. By default,
#'    models are fitted using \code{\link{mistnet_fit_optimx}} using
#'    \code{method = "L-BFGS-B"}.
#' @param ... Additional arguments to \code{\link{mistnet_fit}}
#' @return An object of class \code{network} and subclass \code{mistnet_network}.
#'   This object will contain the original \code{x} and \code{y} matrices,
#'   a list of adjustable parameters (\code{par_list}),
#' @useDynLib mistnet2
#' @importFrom optimx optimx
#' @export
#' @examples
#' set.seed(1)
#'
#' # Load data from the `vegan` package
#' data(mite, mite.env, package = "vegan")
#'
#' # x is a matrix of environmental predictors
#' x = scale(model.matrix(~., data = mite.env)[, -1])
#'
#' # y is a matrix of abundances (counts) for 35 species of mites
#' y = as.matrix(mite)
#'
#' # Fit a neural network with one hidden layer of 10 nodes and an elu
#' # activation function. The response variable has a Poisson distribution
#' # with a log link (exp_activator). The prior distributions for each layer
#' # are each standard normal distributions, and two latent variables are used.
#' net = mistnet(
#'    x = x,
#'    y = y,
#'    n_z = 2,
#'    layers = list(
#'      layer(
#'        activator = elu_activator,
#'        n_nodes = 10,
#'        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
#'      ),
#'      layer(
#'        activator = exp_activator,
#'        n_nodes = ncol(y),
#'        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
#'      )
#'    ),
#'    error_distribution = make_distribution("PO")
#' )
#'
#' print(net)
#'
#' # show the model's predictions for each layer
#' str(feedforward(net, par = unlist(net$par_list)))
#'
#' # Calculate the log-likelihood for each observation under the fitted model
#' log_prob(net, par = unlist(net$par_list), include_penalties = FALSE)
#'
#' # Include penalty terms from the prior to calculate the log-posterior instead
#' log_prob(net, par = unlist(net$par_list), include_penalties = TRUE)

mistnet = function(
  x,
  y,
  n_z,
  layers,
  error_distribution,
  z_prior = make_distribution("NO", mu = 0, sigma = 1),
  fit = TRUE,
  mistnet_optimizer = mistnet_fit_optimx,
  ...
){

  if (is(layers, layer)) {

  }

  n_layers = length(layers)

  n = nrow(x)
  n_x = ncol(x)
  n_y = ncol(y)

  weight_dims = numeric(n_layers + 1)
  weight_dims[1] = n_x + n_z              # nrow of first weight matrix
  for (i in 1:n_layers) {
    weight_dims[i + 1] = layers[[i]]$n_nodes
  }

  activators = lapply(layers, function(layer) layer$activator)
  weight_priors = lapply(layers, function(layer) layer$weight_prior)

  weights = lapply(
    1:n_layers,
    function(i){
      initialize_weights(layers[[i]],
                         n_in = weight_dims[i],
                         n_out = weight_dims[i + 1]
      )
    }
  )


  biases = lapply(
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

  network = list(
    x = x,
    y = y,
    par_list = list(
      z = matrix(draw_samples(z_prior, n = n * n_z), nrow = n, ncol = n_z),
      weights = weights,
      biases = biases
    ),
    activators = activators,
    weight_priors = weight_priors,
    z_prior = z_prior,
    error_distribution = error_distribution
  )
  class(network) = c("mistnet_network", "network")


  if (fit) {
    network = mistnet_fit(network, ...)
  } else {
    network$optimization_results = list()
  }

  return(network)
}
