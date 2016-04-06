#' Normal distribution with empirical mean and variance
#'
#' This distribution produces a Gaussian distribution whose moments are
#' dynamically set to match \code{mean(x)} and \code{sd(x)}. It is used for
#' empirical-Bayesian descriptions of the distribution of \code{x} (similar to
#' the distribution of coefficients in a mixed effects model).
#' @param x vector of quantiles
#' @param mu vector of location parameters (not used)
#' @param n number of samples
#' @param log logical; if TRUE, probabilities p are instead given as log(p)
#' @export
eNO = function() {
  list(
    family = c("eNO", "Empirical Normal"),
    parameters = list(
      mu = TRUE # Not actually used, but some code may assume it's there
    ),
    dldx = function(x, ...){
      dldx_list$NO(x, mu = mean(x), sigma = sd(x))
    }
  )
}

#' @export
#' @rdname eNO
deNO = function(x, log, ...) {
  dnorm(x, mean = mean(x), sd = sd(x), log = log)
}

#' @export
#' @rdname eNO
reNO = function(n, x, ...) {
  stop("sampling is not defined for the empirical Normal distribution")
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
      family = c("IU", "imporper uniform"),
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
