#' Make an \code{error_distribution} from a gamlss distribution
#'
#' @param abbreviation An abbreviation matching a \link[gamlss.dist]{gamlss.family},
#'    e.g. "NO" for the normal distribution or "ZIP" for the zero-inflated
#'    Poisson distribution
#' @param ... Additional arguments, possibly including \code{bd} (binomial denominator),
#'    \code{sigma} (scale), \code{nu} (shape), or \code{tau} (shape),
#'    depending on the distribution
#' @return An \code{error_distribution} object, consisting of the following
#'    functions:
#' \itemize{
#'    \item{\code{log_density(x, mu)}: log probability of the distribution with
#'        location parameter \code{mu} evaluated at \code{x}.}
#'    \item{\code{sample(n, mu)}: draw \code{n} random samples from the distribution
#'        with location parameter \code{mu}.}
#'    \item{Functions for calculating partial derivatives evaluated at \code{x}
#'        with regard to mu (\code{dldm}), sigma (\code{dldd}), nu
#'        (\code{dldv}), and tau (\code{dldt}). For a few distributions,
#'        \code{dldx}} is also defined: these distributions can be used as
#'        prior distributions on the model's weights.
#' }
#' @import gamlss.dist
#' @aliases error_distribution
#' @export
#' @examples
#' distribution = make_gamlss_distribution("NO", sigma = 1/3)
#'
#' # Sample 10 random values with mean=2 (and sigma=1/3 as defined above)
#' samples = distribution$sample(n = 10, mu = 2)
#'
#' # find the log_density of those samples under a distribution with mean=1
#' distribution$log_density(samples, mu = 1)
#'
#' # The gradient of the log_density with respect to the mean indicates that
#' # the log_density would be higher if mu were larger (more positive)
#' distribution$dldm(samples, mu = 1)
#'
#' # The gradient with respect to x shows that the log_density would also be
#' # higher if we reduced the values of x (more negative)
#' distribution$dldx(samples, mu = 1)
#'
#' # The gradient with respect to x isn't defined for all distributions,
#' # however. For example, this code would return an error that
#' # " gradient with respect to x (dldx) is not defined for the distribution PO"
#' \dontrun{
#' another_distribution = make_gamlss_distribution("PO")
#' another_distribution$dldx(x = 1:10, mu = 1)
#' }
make_gamlss_distribution = function(abbreviation, ...){
  family_object = get(abbreviation, mode = "function")()

  included_parameters = names(family_object$parameters)

  # Confirm that all the needed parameters (except mu) are included in `...`
  for(parameter in included_parameters){
    if(parameter == "mu" | parameter %in% names(list(...))){
      # all is well
    }else{
      stop(parameter, " is required for the `", abbreviation, "` distribution")
    }
  }

  # Confirm that mu is *not* given. If it's part of `...`, it could get
  # passed in weird ways if the user forgets to add mu to one of the objects's
  # functions.
  if("mu" %in% names(list(...))){
    stop("mu should not be given when making an error_distribution")
  }

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

        r(n = n, mu = mu, ...)
      },
      dldm = get_grad(family_object, "mu", ...),
      dldd = get_grad(family_object, "sigma", ...),
      dldv = get_grad(family_object, "nu", ...),
      dldt = get_grad(family_object, "tau", ...),
      dldx = get_grad(family_object, "x", ...)
    ),
    class = "error_distribution"
  )
}


get_grad = function(
  family_object,
  param_name,
  ...
){
  abbreviation = family_object$family[[1]]

  # One-letter abbreviations for parameters used in names of gradients
  # (e.g. dldd for sigma)
  param_abbreviation = switch(
    param_name,
    mu    = "m",
    sigma = "d",
    tau   = "t",
    nu    = "v",
    x     = "x"
  )

  grad_name = paste0("dld", param_abbreviation)

  if(param_name %in% names(family_object$parameters)){
    # Find the gradient in the family_object
    out = function(x, mu){
      family_object[[grad_name]](y = x, mu = mu, ...)
    }
  }else{
    if(!is.null(dldx[[abbreviation]])){
      # Find the gradient in the dldx list below
      out = function(x, mu){
        dldx[[abbreviation]](x = x, mu = mu, ...)
      }
    }else{
      out = function(...){
        # No gradient defined
        stop(
          "gradient with respect to ",
          param_name,
          " (",
          grad_name,
          ") is not defined for the distribution '",
          abbreviation,
          "'"
        )
      }
    }
  }

  return(out)
}

#' Partial derivatives with respect to x
#'
#' These gradients are used for optimizing x with respect to the log-prior density
#' (as opposed to optimizing the distribution's parameters with respect to x, as with dldm
#' and the other derivatives available from \code{\link{error_distribution}}
#' objects). Mostly used internally by \code{\link{make_gamlss_distribution}}.
#' @format A list of functions, named according to their
#' \link[gamlss.dist]{gamlss.family} abbreviations (e.g. "NO" for the normal
#' distribution).  Additional functions can be added to the list by the user,
#' making it possible to use the corresponding distributions as priors.
#'
#' Currently, the only distribution with built-in support is
#' "\link[gamlss.dist]{NO}", the normal distribution
#' @export
#' @examples
#' # find the gradient with respect to x using dldx
#' exact_gradient = dldx$NO(x = 1, mu = 2, sigma = pi)
#'
#' # Compare with the numerical gradient for the same distribution
#' numerical_gradient = numDeriv::grad(function(x)dnorm(x = x, mean = 2, sd = pi, log = TRUE), x = 1)
#'
#' stopifnot(all.equal(exact_gradient, numerical_gradient))
dldx = list(
  NO = function(x, mu, sigma){
    -(x - mu) / sigma^2
  }
)
