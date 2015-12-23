#' Improper uniform distribution
#'
#' This distribution produces a flat prior with no bounds.  Its gradient and
#' log-density are always zero. It's improper because its integral is not one
#' (and is not finite). Sampling from the distribution is not defined, so calling
#' rIU results in an error.
#' @param x vector of quantiles
#' @param mu vector of location parameters (not used)
#' @param n number of samples
#' @param log logical; if TRUE, probabilities p are given as log(p)
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
