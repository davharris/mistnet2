#' Calculate the log probability density of an object
#'
#' @param object an object
#' @param ... additional arguments to be passed to other methods
#'
#' @export
#' @seealso \code{\link{log_prob.distribution}},
#'    \code{\link{log_prob.mistnet_network}}
log_prob = function(object, ...){
  UseMethod("log_prob")
}

#' Calculate the log probability of a distribution
#' @param object an \code{\link{distribution}} object
#' @param ... additional arguments passed to \code{\link{get_values}}
#' @export
log_prob.distribution = function(object, ...){
  values = get_values(object, log = TRUE, ...)

  # Some density functions, like gamlss.dist::dBI, give cryptic error messages
  # in response to missing values. This warning should help explain what's
  # wrong.
  if (any(is.na(unlist(values)))) {
    warning("non-finite values detected")
  }

  do.call(object$d, values)
}


#' Calculate the log-likelihood of a network object
#' @param object A \code{network} object
#' @param state An optional \code{network_state} object. If not included,
#'    state will be calculated by \code{\link{feedforward}}.
#' @param include_penalties Should penalty terms (e.g. from prior distributions)
#' be included in the log-density?
#' @param par a vector of network parameters
#' @param ... Additional arguments (currently not used.)
#' @export
log_prob.mistnet_network = function(object, state, par, include_penalties,
                                       ...){
  if (missing(state)) {
    state = feedforward(object, par)
  }

  if (missing(par)) {
    parameters = object$par_list
  } else {
    parameters = build_par_list(par = par, par_list = object$par_list)
  }

  error_distribution = object$error_distribution

  # Replace object's error distribution parameters with adjusted values
  # wherever adjusted values exist
  for (name in names(error_distribution$family_parameters)) {
    if (name %in% names(parameters$error_distribution_par)) {
      error_distribution$family_parameters[[name]] = parameters$error_distribution_par[[name]]
    }
  }


  out = log_prob(
    error_distribution,
    x = object$y,
    mu = state$outputs[[length(state$outputs)]]
  )

  if (include_penalties) {
    weight_penalties = sapply(
      1:length(object$weight_priors),
      function(i){
        sum(log_prob(object$weight_priors[[i]], x = parameters$weights[[i]], ...))
      }
    )

    z_penalties = log_prob(object$z_prior, x = parameters$z, ...)

    total_penalty = sum(weight_penalties) + sum(z_penalties)

    # The sum of the returned values needs to equal
    # sum(log_prob.distribution) + sum(penalties). Dividing by
    # length(out) prevents double-counting when the output is summed up later.
    return(out + total_penalty / length(out))
  } else {
    return(out)
  }
}
