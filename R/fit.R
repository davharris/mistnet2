#'Fit a mistnet model
#' @param network A \code{network} object, as produced by the
#'    \code{\link{mistnet}} function
#' @param mistnet_optimizer A \code{function} for maximizing the penalized
#'    likelihood of a mistnet model. By default, this is \code{\link{mistnet_fit_optimx}}.
#' @param ... Additional arguments passed to the \code{mistnet_optimizer}
#' @return A \code{network} object, like the one produced by \code{\link{mistnet}},
#'    but with updated parameters and with an extra list containing the
#'    \code{optimization_results}
#' @export
mistnet_fit = function(network, mistnet_optimizer = mistnet_fit_optimx, ...){
  fn = function(par){sum(log_density(network, par = par, include_penalties = TRUE))}
  gr = function(par){unlist(backprop(network, par = par))}

  network = mistnet_optimizer(network, fn = fn, gr = gr, ...)
}

#' Optimize a mistnet model using the \code{\link[optimx]{optimx}} package
#' @param network A \code{network} object, as produced by the
#'    \code{\link{mistnet}} function
#' @param fn A function of \code{par} that returns a scalar describing the
#'    (penalized) log-likelihood
#' @param gr A function of \code{par} that returns a vector containing the
#'    gradient of the (penalized) log-likelihood with respect to all the
#'    parameters in \code{par}.
#' @param method,trace,maxit,starttests,hessian,... Additional arguments passed to
#'    \code{\link[optimx]{optimx}}
#' @export mistnet_fit_optimx
mistnet_fit_optimx = function(
  network,
  fn,
  gr,
  method = "L-BFGS-B",
  trace = 0,
  maxit = 1000,
  starttests = TRUE,
  hessian = FALSE,
  ...
){

  opt = optimx::optimx(
    par = unlist(network$par_skeleton),
    fn = fn,
    gr = gr,
    method = method,
    control = list(
      trace = trace,
      maximize = TRUE,
      starttests = starttests,
      maxit = maxit,
      ...
    ),
    hessian = FALSE,
    ...
  )

  network$optimization_results = list(
    optimizer = "optimx",
    method = method,
    fevals = opt$fevals,
    gevals = opt$gevals,
    convcode = opt$convcode,
    xtimes = opt$xtimes
  )

  network$par_skeleton = relist(coef(opt), network$par_skeleton)

  network
}
