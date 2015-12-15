#' Calculate the log-likelihood of a network object
#' @param object A \code{network} object
#' @param state An optional \code{network_state} object. If not included,
#'    state will be calculated by \code{\link{feedforward}}.
#' @param include_penalties Should penalty terms be included in the
#'    log-likelihood?
#' @param par a vector of parameters
#' @param ... Additional arguments (currently not used.)
log_density = function(network, state, par, include_penalties = FALSE, ...){
  if(missing(state)){
    state = feedforward(network, par)
  }

  out = sum(
    network$error_distribution$log_density(
      network$y,
      mu = state$outputs[[length(state$outputs)]]
    )
  )

  if(include_penalties){
    stop("penalties haven't been implemented yet")
    return(out + network$penalty)
  }else{
   return(out)
  }
}
