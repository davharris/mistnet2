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
#' @param error_distribution Either an \code{\link{error_distribution}} object
#'    or a \link[gamlss.dist]{gamlss.family} abbreviation (such as "NO" for the
#'    normal distribution)
#' @param priors [[Add me]]
#' @param fit Logical. Should the model be fitted or should an untrained model
#'    be returned. Defaults to TRUE
#' @param starttests Should \code{\link[optimx]{optimx}}'s \code{starttests} be
#'    run? Can be useful for identifying errors but is not usually needed.
#' @param sigma (optional) scale parameter passed to
#'    \code{\link{make_gamlss_distribution}}
#' @param tau (optional) shape parameter passed to
#'    \code{\link{make_gamlss_distribution}}
#' @param nu (optional) shape parameter passed to
#'    \code{\link{make_gamlss_distribution}}
#' @param bd (optional) binomial denominator passed to
#'    \code{\link{make_gamlss_distribution}}
#' @param ... Additional arguments to fit
#' @return A \code{network} object
#' @useDynLib mistnet2
#' @importFrom optimx optimx
#' @export
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
  sigma = NULL,
  tau = NULL,
  nu = NULL,
  bd = NULL,
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
    error_distribution = make_gamlss_distribution(
      error_distribution,
      sigma = sigma,
      tau = tau,
      nu = nu,
      bd = bd
    )
  )
  class(network) = "network"


  if (fit) {
    network = mistnet_fit(network, ...)
  } else {
    network$optimization_results = list()
  }

  return(network)
}
