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
#'   a list of adjustable parameters (\code{par_list}), [[etc.]]
#' @useDynLib mistnet2
#' @importFrom optimx optimx
#' @importFrom assertthat assert_that is.scalar is.count are_equal noNA is.flag
#' @importFrom purrr every
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
  if (is(layers, "layer")) {
    # Correct for easy mistake with single-layer networks
    layers = list(layers)
  }

  assert_that(is.matrix(x), is.numeric(x), noNA(x))
  assert_that(is.matrix(y), is.numeric(y), noNA(y))
  assert_that(are_equal(nrow(x), nrow(y)))
  assert_that(every(layers, is, "layer"))
  assert_that(is.count(n_z))
  assert_that(are_equal(layers[[length(layers)]]$n_nodes, ncol(y)))
  assert_that(is(z_prior, "distribution"))
  assert_that(is(error_distribution, "distribution"))
  assert_that(is.flag(fit))

  activators = lapply(layers, function(layer) layer$activator)
  n = nrow(x)

  network = list(
    x = x,
    y = y,
    par_list = list(
      z = matrix(draw_samples(z_prior, n = n * n_z), nrow = n, ncol = n_z),
      weights = make_weight_list(n_x = ncol(x), n_z = n_z, layers = layers),
      biases = make_bias_list(layers = layers,
                              error_distribution = error_distribution,
                              activators = activators,
                              y = y)
    ),
    activators = activators,
    weight_priors = lapply(layers, function(layer) layer$weight_prior),
    z_prior = z_prior,
    error_distribution = error_distribution,
    optimization_results = list()
  )
  class(network) = c("mistnet_network", "network")

  if (fit) {
    network = mistnet_fit(network, ...)
  }

  return(network)
}
