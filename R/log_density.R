log_density = function(object, ...){
  UseMethod("log_density", object)
}


log_density.error_distribution = function(distribution, adjusted_values, ...){
  do.call(
    distribution$d,
    get_values(distribution, adjusted_values, ..., log = TRUE)
  )
}


#' Calculate the log-likelihood of a network object
#' @param network A \code{network} object
#' @param state An optional \code{network_state} object. If not included,
#'    state will be calculated by \code{\link{feedforward}}.
#' @param include_penalties Should penalty terms (e.g. from prior distributions)
#' be included in the log-density?
#' @param par a vector of network parameters
#' @param ... Additional arguments (currently not used.)
log_density.network = function(network, state, par, include_penalties = FALSE, ...){
  if(missing(state)){
    state = feedforward(network, par)
  }

  out = network$error_distribution$log_density(
    network$y,
    mu = state$outputs[[length(state$outputs)]]
  )

  if(include_penalties){
    stop("penalties haven't been implemented yet")
    return(out + network$penalty)
  }else{
    return(out)
  }
}
