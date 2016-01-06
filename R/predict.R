#' Make predictions from a trained network
#'
#' Predictions are made by feeding new data through the network (see
#' \code{\link{feedforward}}.  For networks with latent variables
#' (\code{mistnet_network}s), these variables are sampled randomly from their
#' prior distribution and then each batch of samples is fed through the network
#'
#' @param object an object of class \code{network} or \code{mistnet_network}
#' @param newdata a matrix with the same columns as \code{x}
#' @param ... further arguments passed to or from other methods.
#' @return For generic networks, a \code{network_state} object, as in
#'    \code{\link{feedforward}}. For mistnet networks, a list of such objects,
#'    a list of such object (one for each Monte Carlo sample)
#' @importFrom assertthat assert_that noNA
#' @export
predict.network = function(object, newdata, ...){
  assert_that(is.matrix(newdata), is.numeric(newdata), noNA(newdata))
  assert_that(are_equal(ncol(object$x), ncol(newdata)))

  object$x = newdata
  feedforward(object)
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

  feedforward_with_samples(object, z_samples)
}
