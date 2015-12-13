#' Calculate the log-likelihood of a network object
#' @param object A \code{network} object
#' @param state An optional \code{network_state} object. If not included,
#'    state will be calculated by \code{\link{feedforward}}.
#' @param include_penalties Should penalty terms be included in the
#'    log-likelihood?
#' @param ... Additional arguments (currently not used.)
logLik.network = function(object, state, include_penalties = FALSE, ...){
  if(missing(state)){
    state = feedforward(object)
  }

  out = sum(
    object$error_distribution$logLik(
      # mu comes from the output of the last layer
      mu = state$outputs[[length(state$outputs)]]
    )
  )

  if(include_penalties){
    stop("penalties haven't been implemented yet")
    return(out + object$penalty)
  }else{
   return(out)
  }
}
