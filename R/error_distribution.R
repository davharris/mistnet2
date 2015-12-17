#' Make an \code{error_distribution} from a gamlss distribution
#'
#' @param family_function Either a character vector containing the name of a
#'    function that produces a \code{\link[gamlss.dist]{gamlss.family}} (e.g.
#'    \code{family_function = "\link[gamlss.dist]{NO}"} for the normal
#'    distribution), or a user-created function that returns an object with
#'    the same structure. [[Add explanation of how to do this: see dEXAMPLE in
#'    test-distributions.R]]
#'    \code{\link[gamlss.dist]{gamlss.family}}.
#' @param ... Additional arguments, possibly including \code{bd} (binomial denominator),
#'    \code{sigma} (scale), \code{nu} (shape), or \code{tau} (shape),
#'    depending on the distribution. These must be named, and partial matching
#'    is not allowed.
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
#'        prior distributions on the model's weights. Users can add \code{dldx}
#'        functions by creating their own distributions that include a function
#'        called \code{dldx}.
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
#'
make_gamlss_distribution = function(family_function, ...){
  if(is(family_function, "family")){
    stop("family_function should refer to a function that creates a family\nobject, not the object itself")
  }

  if(is.function(family_function)){
    family_object = family_function()
    abbreviation = family_object$family[[1]]
  }else{
    if(is.character(family_function)){
      abbreviation = family_function
      family_object = get(abbreviation, mode = "function")()
    }
  }

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

  # Get the dABB function, where ABB is the abbreviation.
  # Can't specify function location without attaching whole gamlss.dist
  # package??
  d = get(paste0("d", abbreviation), mode = "function")

  structure(
    c(
      log_density = function(x, mu){
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


  out = function(x, mu){
    family_object[[grad_name]](y = x, mu = mu, ...)
  }

  # dldx won't always be inside the family object
  if(is.null(family_object[[grad_name]])){

    # Try looking for it in the package's dldx list
    if(!is.null(dldx_list[[abbreviation]])){
      out = function(x, mu){
        dldx_list[[abbreviation]](x = x, mu = mu, ...)
      }
    }else{
      out = function(x, mu){
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


dldx_list = list(
  NO = function(x, mu, sigma){
    -(x - mu) / sigma^2
  }
)


#' Improper uniform distribution
#'
#' This distribution produces a flat prior with no bounds.  Its gradient and
#' log-density are always zero.
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
      dldx = function(y, mu){
        rep(0, length(y))
      }
    ),
    class = "family"
  )
}

# density function (only defined for log == TRUE).
# Since the prior is improper, it doesn't have to sum to one and
# can return any constant: zero is most convenient because it doesn't
# affect the posterior
dIU = function(x, mu, log){
  stopifnot(log)
  rep(0, max(length(x), length(mu)))
}
