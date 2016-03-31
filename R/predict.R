#' Make predictions from a trained network
#'
#' Predictions are made by feeding new data through the network (see
#' \code{\link{feedforward}}.  For networks with latent variables
#' (\code{mistnet_network}s), these variables are sampled randomly from their
#' prior distribution and then each batch of samples is fed through the network
#'
#' @param object an object of class \code{network} or \code{mistnet_network}
#' @param newdata a matrix with the same columns as \code{x}
#' @param full_state should the predictions include the full list of
#'   \code{network_state} produced by \code{\link{feedforward}} or just the
#'   final layer's outputs? By default, only the final layer's outputs are
#'   returned.
#' @param ... further arguments passed to or from other methods.
#' @return For generic networks, a matrix of output variables (one row per
#'    row in \code{newdata}, one column per node in the network's final layer).
#'    For mistnet networks, a three-dimensional array, with the first two
#'    indices matching the generic case and with the third dimension indexing
#'    the Monte Carlo samples.
#'
#'    Alternatively, if \code{} is \code{TRUE}, a \code{network_state} object
#'    is returned, as in \code{\link{feedforward}}. For mistnet networks,
#'    a list of such objects, a list of such objects (one for each Monte Carlo
#'    sample) is returned.
#' @importFrom assertthat assert_that noNA
#' @export
predict.network = function(object, newdata, full_state = FALSE, ...){
  assert_that(is.matrix(newdata), is.numeric(newdata), noNA(newdata))
  assert_that(are_equal(ncol(object$x), ncol(newdata)))

  object$x = newdata
  out = feedforward(object)

  if (full_state) {
    return(out)
  } else {
    return(out$outputs[[length(out$outputs)]])
  }
}

#' @rdname predict.network
#' @param n_samples (for \code{mistnet_networks} only). The number of Monte
#'   Carlo samples to use
#'
#' @export
predict.mistnet_network = function(
  object,
  newdata,
  n_samples,
  full_state = FALSE,
  ...
){
  assert_that(is.matrix(newdata), is.numeric(newdata), noNA(newdata))
  assert_that(are_equal(ncol(object$x), ncol(newdata)))

  object$x = newdata

  n_z = ncol(object$par_list$z)

  # Draw n_samples sample matrices from z's prior distribution
  z_samples = replicate(
    n_samples,
    {
      matrix(
        draw_samples(object$z_prior, n = nrow(newdata) * n_z),
        nrow = nrow(newdata),
        ncol = n_z
      )
    },
    simplify = FALSE
  )

  out_list = feedforward_with_samples(object, z_samples)

  if (full_state) {
    out = out_list
  } else {
    out = array(NA, c(nrow(newdata), ncol(object$y), n_samples))
    for (i in 1:n_samples) {
      out[ , , i] = out_list[[i]]$outputs[[length(out_list[[i]]$outputs)]]
    }
  }

  out
}
