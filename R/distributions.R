#' Normal distribution with empirical mean and variance
#'
#' This distribution produces a Gaussian distribution whose moments are
#' dynamically set to match \code{mean(x)} and \code{sd(x)}. It is used for
#' empirical-Bayesian descriptions of the distribution of \code{x} (similar to
#' the distribution of coefficients in a mixed effects model).
#'
#' @param by one of \code{"row"}, \code{"col"}, or \code{"mat"}. Should
#'     the empirical moments be estimated for each row, each column, or for the
#'     full matrix?
#' @param x vector of quantiles
#' @param n number of samples
#' @param log logical; if TRUE, probabilities p are instead given as log(p)
#' @param ... additional arguments passed to or from other methods (currently
#'     not used)
#' @export
ENO = function(by) {
  list(
    family = c("ENO", "Empirical Normal"),
    parameters = list(
      by = TRUE
    ),
    dldx = function(x, by, ...){
      # emp_apply (the apply function for empirical distributions) is defined below
      dldx_list$NO(x, mu = emp_apply(x, mean, by), sigma = emp_apply(x, sd, by))
    }
  )
}

#' @export
#' @rdname ENO
dENO = function(x, log, by, ...) {
  dnorm(x, mean = emp_apply(x, mean, by), sd = emp_apply(x, sd, by), log = log)
}

#' @export
#' @rdname ENO
rENO = function(n, x, ...) {
  stop("sampling is not defined for the empirical Normal distribution")
}


# Apply function for empirical distributions: apply a function to each row,
# each column, or the whole matrix, then expand the result back up to the size
# of the original matrix.
# Note that `f` should return a scalar!
emp_apply = function(x, f, by){
  switch(
    by,
    row = matrix(apply(x, 1, f), nrow = nrow(x), ncol = ncol(x), byrow = FALSE),
    col = matrix(apply(x, 2, f), nrow = nrow(x), ncol = ncol(x), byrow = TRUE),
    mat = matrix(f(x), nrow = nrow(x), ncol = ncol(x))
  )
}


dldx_list = list(
  NO = function(x, mu, sigma){
    -(x - mu) / sigma^2
  }
)

#' Improper uniform distribution
#'
#' This distribution produces a flat prior with no bounds.  Its gradient and
#' log-density are always zero. It's improper because its integral is not one
#' (and is not finite). Sampling from the distribution is not defined, so calling
#' rIU results in an error.
#' @param x vector of quantiles
#' @param mu vector of location parameters (not used)
#' @param n number of samples
#' @param log logical; if TRUE, probabilities p are instead given as log(p)
#' @export
IU = function(){
  structure(
    list(
      family = c("IU", "improper uniform"),
      parameters = list(
        mu = TRUE # All distributions currently need mu
      ),
      dldm = function(y, mu){
        rep(0, length(mu))
      },
      dldx = function(x, mu){
        rep(0, length(x))
      }
    ),
    class = "family"
  )
}

#' @export
#' @rdname IU
dIU = function(x, mu, log){
  if (log) {
    rep(0, max(length(x), length(mu)))
  } else {
    rep(1, max(length(x), length(mu)))
  }

}

#' @export
#' @rdname IU
rIU = function(n, mu){
  stop("Sampling is not defined for improper distributions")
}
