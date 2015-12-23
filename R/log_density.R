#' Calculate the log probability density of an object
#'
#' @param object an object
#' @param ... additional arguments to be passed to other methods
#'
#' @export
#' @seealso \code{\link{log_density.error_distribution}},
#'    \code{\link{log_density.network}}
log_density = function(object, ...){
  UseMethod("log_density", object)
}


#' @export
log_density.error_distribution = function(object, ...){
  do.call(
    object$d,
    get_values(object, log = TRUE, ...)
  )
}


#' Calculate the log-likelihood of a network object
#' @param object A \code{network} object
#' @param state An optional \code{network_state} object. If not included,
#'    state will be calculated by \code{\link{feedforward}}.
#' @param include_penalties Should penalty terms (e.g. from prior distributions)
#' be included in the log-density?
#' @param par a vector of network parameters
#' @param ... Additional arguments (currently not used.)
log_density.network = function(object, state, par, include_penalties = FALSE, ...){
  if(missing(state)){
    state = feedforward(object, par)
  }

  out = log_density(
    object$error_distribution,
    x = object$y,
    mu = state$outputs[[length(state$outputs)]]
  )

  if(include_penalties){
    stop("penalties haven't been implemented yet")
    return(out + object$penalty)
  }else{
    return(out)
  }
}
