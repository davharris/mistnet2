#' Build and fit a neural network with random effects
#' @param x A numeric matrix of predictor variables
#' @param y A numeric or integer matrix of response variables
#' @param n_z, The number of latent random variables to include as predictors
#'    alongside x
#' @param activators A list of \code{\link{activator}} objects, one per
#'    network layer
#' @param n_hidden An integer vector determining the number of hidden nodes in
#'    each hidden layer. Its length should be one less than that of the
#'    \code{activators} list.
#' @param error_distribution An \code{\link{error_distribution}} object
#' @param priors [[Add me]]
#' @param fit Logical. Should the model be fitted or should an untrained model
#'    be returned. Defaults to TRUE
#' @param starttests Should \code{\link[optimx]{optimx}}'s \code{starttests} be
#'    run? Can be useful for identifying errors but is not usually needed.
#' @param ... Additional arguments to fit
#' @return A \code{network} object
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
#'    n_hidden = 10,
#'    activators = list(elu_activator, exp_activator),
#'    priors = list(
#'      make_gamlss_distribution("NO", mu = 0, sigma = 1),
#'      make_gamlss_distribution("NO", mu = 0, sigma = 1)
#'    ),
#'    error_distribution = make_gamlss_distribution("PO")
#' )
#'
#' print(net)
#'
#' # show the model's predictions for each layer
#' str(feedforward(net, par = unlist(net$par_skeleton)))
#'
#' # Calculate the log-likelihood for each observation under the fitted model
#' log_density(net, par = unlist(net$par_skeleton), include_penalties = FALSE)
#'
#' # Include penalty terms from the prior to calculate the log-posterior instead
#' log_density(net, par = unlist(net$par_skeleton), include_penalties = TRUE)

mistnet = function(
  x,
  y,
  n_z,
  activators,
  n_hidden,
  error_distribution,
  priors,
  fit = TRUE,
  starttests = FALSE,
  ...
){
  stopifnot(length(n_hidden) == (length(activators) - 1))
  stopifnot(length(priors) == length(activators))

  n_layers = length(activators)

  n = nrow(x)
  n_x = ncol(x)
  n_y = ncol(y)


  weight_dims = numeric(n_layers + 1)
  weight_dims[1] = n_x + n_z              # nrow of first weight matrix
  weight_dims[length(weight_dims)] = n_y  # ncol of last weight matrix
  if (n_layers > 1) {
    for (i in 2:n_layers) {
      weight_dims[i] = n_hidden[i - 1]
    }
  }

  network = list(
    x = x,
    y = y,
    par_skeleton = list(
      z = matrix(rnorm(n * n_z, sd = .5), nrow = n, ncol = n_z),
      weights = lapply(
        1:n_layers,
        function(i){
          matrix(
            rnorm(
              prod(weight_dims[c(i, i + 1)]),
              sd = .5
            ),
            nrow = weight_dims[i],
            ncol = weight_dims[i + 1]
          )
        }
      ),
      biases = c(
        lapply(n_hidden, function(n){rnorm(n, sd = 0.5)}),
        list(rnorm(n_y, sd = .5))
      )
    ),
    activators = activators,
    priors = priors,
    error_distribution = error_distribution
  )
  class(network) = "network"


  if (fit) {
    network = mistnet_fit(network, ...)
  } else {
    network$optimization_results = list()
  }

  return(network)
}
