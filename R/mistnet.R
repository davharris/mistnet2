#' Build and fit a neural network with random effects
#' @param x A numeric matrix of predictor variables
#' @param y A numeric or integer matrix of response variables
#' @param n_z, The number of latent random variables to include as predictors
#'    alongside x
#' @param activators A list of \code{\link{activator}} functions, one per
#'    network layer
#' @param error_distribution Either an \code{\link{error_distribution}} object
#'    or a \link[gamlss.dist]{gamlss.family} abbreviation (such as "NO" for the
#'    normal distribution)
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
#' @return A \code{network} object
#' @useDynLib mistnet2
#' @importFrom optimx optimx
#' @export
mistnet = function(
  x,
  y,
  n_z,
  activators,
  error_distribution,
  fit = TRUE,
  starttests = FALSE,
  sigma,
  tau,
  nu,
  bd
){
  # TODO: Figure out weight/bias/latent initialization

  n = nrow(x)
  n_x = ncol(x)
  n_y = ncol(y)

  network = list(
    x = x,
    y = y,
    par_skeleton = list(
      z = matrix(rnorm(n * n_z, sd = .5), nrow = n, ncol = n_z),
      weights = list(
        matrix(rnorm((n_x + n_z) * n_y, sd = .5), nrow = n_x + n_z, ncol = n_y)
      ),
      biases = list(
        rnorm(n_y, sd = .5)
      )
    ),
    activators = activators,
    error_distribution = if(is.character(error_distribution)){
      make_gamlss_distribution(
        error_distribution,
        sigma = sigma,
        tau = tau,
        nu = nu,
        bd = bd
      )
    }else{
      error_distribution
    }
  )
  class(network) = "network"


  if(fit){
    opt = optimx::optimx(
      par = unlist(network$par_skeleton),
      fn = function(par){logLik.network(network, par = par)},
      gr = function(par){unlist(backprop(network, par = par))},
      method = "L-BFGS-B",
      control = list(trace = 0, maximize = TRUE, starttests = starttests, maxit = 1000),
      hessian = FALSE
    )

    network$optimization_results = list(
      fevals = opt$fevals,
      gevals = opt$gevals,
      convcode = opt$convcode,
      xtimes = opt$xtimes
    )

    network$par_skeleton = relist(coef(opt), network$par_skeleton)
  }

  return(network)
}

