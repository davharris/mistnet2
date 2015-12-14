#' @import gamlss.dist
#' @export
make_gamlss_distribution = function(abbreviation, ...){
  family_object = get(abbreviation, mode = "function")()

  structure(
    c(
      log_density = function(x, mu){
        # Get the dABB function, where ABB is the abbreviation.
        # Can't specify function location without attaching whole gamlss.dist
        # package??
        d = get(paste0("d", abbreviation), mode = "function")

        # log density at x with location mu and additional arguments from `...`
        d(x = x, mu = mu, log = TRUE, ...)
      },
      sample = function(n, mu){
        # get the rABB function, where ABB is the abbreviation
        r = get(paste0("r", abbreviation), mode = "function")

        #
        r(n = n, mu = mu, ...)
      },
      dldm = function(x, mu){
        family_object$dldm(y = x, mu = mu, ...)
      },
      dldd = function(x, mu){
        family_object$dldd(y = x, mu = mu, ...)
      },
      dldv = function(x, mu){
        family_object$dldv(y = x, mu = mu, ...)
      },
      dldt = function(x, mu){
        family_object$dldt(y = x, mu = mu, ...)
      }
    ),
    class = "error_distribution"
  )
}
