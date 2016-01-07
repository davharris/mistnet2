#' Make an \code{distribution} from a gamlss distribution
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
#' @return An \code{distribution} object, consisting of the following
#'    functions:
#' \itemize{
#'    \item{\code{log_prob(x, mu)}: log probability of the distribution with
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
#' @aliases distribution
#' @export
#' @examples
#' distribution = make_distribution("NO", sigma = 1/3)
#'
#' # Sample 10 random values with mean=2 (and sigma=1/3 as defined above)
#' samples = draw_samples(distribution, n = 10, mu = 2)
#'
#' # find the log_prob of those samples under a distribution with mean=1
#' log_prob(distribution, x = samples, mu = 1)
#'
#' # The gradient of the log_prob with respect to the mean indicates that
#' # the log_prob would be higher if mu were larger (more positive)
#' grad(distribution, "mu", y = samples, mu = 1)
#'
#' # The gradient with respect to x shows that the log_prob would also be
#' # higher if we reduced the values of x (more negative)
#' grad(distribution, "x", x = samples, mu = 1)
#'
#' # The gradient with respect to x isn't defined for all distributions,
#' # however. For example, this code would return an error that
#' # " gradient with respect to x (dldx) is not defined for the distribution PO"
#' \dontrun{
#' another_distribution = make_distribution("PO")
#' another_distribution$dldx(x = 1:10, mu = 1)
#' }
#'
make_distribution = function(family_function, ...){
  if (is(family_function, "family")) {
    stop("family_function should refer to a function that creates a family\nobject, not the object itself")
  }

  if (is.function(family_function)) {
    # Call the function and pull out the abbreviation
    family_object = family_function()
    abbreviation = family_object$family[[1]]
  } else {
    if (is.character(family_function)) {
      # Use the abbreviation to grab the function and call it
      abbreviation = family_function
      family_object = get(abbreviation, mode = "function")()
    }
  }

  family_parameters = family_object$parameters

  # Binomial denominator acts like a parameter, but isn't included
  # in the gamlss distributions.
  if ("bd" %in% names(formals(family_object$dldm))) {
    family_parameters$bd = NA
  }

  dots = list(...)

  for (param_name in names(family_parameters)) {
    if (param_name %in% names(dots)) {
      family_parameters[[param_name]] = dots[[param_name]]
    } else {
      if (param_name == "mu") {
        # mu doesn't always need to be included in `dots` because network error
        # distributions get mu values from the network's final layer instead
        family_parameters[[param_name]] = NA
      }else{
        stop(param_name, " is required for the `", abbreviation,
             "` distribution")
      }
    }
  }

  # Get the `dABB` and `rABB` functions, where ABB is the abbreviation.
  d = get(paste0("d", abbreviation), mode = "function")
  r = get(paste0("r", abbreviation), mode = "function")

  structure(
    list(
      family = family_object$family,
      family_parameters = family_parameters,
      d = d,
      r = r,
      dldm = family_object$dldm,
      dldd = family_object$dldd,
      dldv = family_object$dldv,
      dldt = family_object$dldt,
      dldx = get_dldx(family_object)
    ),
    class = "distribution"
  )
}

#' Calculate the gradient of a distribution
#' @param distribution An \code{\link{distribution}} object
#' @param name One of \code{"mu"}, \code{"sigma"}, \code{"nu"}, \code{"tau"}, or \code{"x"}
#' @param ... additional arguments passed to \code{\link{get_values}}
#' @export
grad = function(distribution, name, ...){
  f = switch(
    name,
    mu = distribution$dldm,
    sigma = distribution$dldd,
    nu = distribution$dldv,
    tau = distribution$dldt,
    x = distribution$dldx
  )

  do.call(f, get_values(distribution, ...))
}

# Calculate error gradients for all the parameters
all_error_grads = function(error_distribution, error_distribution_par, y, mu) {

  # All the gradient calculations will require these arguments
  arg_list = c(
    list(
      distribution = error_distribution,
      y = y,
      mu = mu
    ),
    error_distribution_par[names(error_distribution_par) != "mu"] # mu is handled above, not here
  )

  par_names = c("mu", names(error_distribution_par))

  # Loop through the parameters and calculate gradients for each
  out = lapply(
    par_names,
    function(name){
      out = do.call(grad, c(arg_list, name = name))
      if (name != "mu") {
        # Currently assuming scalar values for adjustable parameters!
        out = sum(out)
      }

      out
    }
  )

  structure(out, names = par_names)
}


#' Get parameter values from a distribution object
#'
#' @param distribution a distribution object
#' @param ... additional arguments, which will override values contained within
#'    the \code{distribution}
#' @export
get_values = function(distribution, ...){

  values = list(...)

  for (param_name in names(distribution$family_parameters)) {
    if (param_name %in% names(values)) {
      # Do nothing: a value has already been provided
    } else {
      # pull the value from the distribution object itself
      values[[param_name]] = distribution$family_parameters[[param_name]]
    }
  }

  return(values)
}


get_dldx = function(family_object){
  abbreviation = family_object$family[[1]]

  # User-defined objects should contain dldx if needed
  out = family_object$dldx

  # gamlss-based objects won't contain dldx
  if (is.null(family_object$dldx)) {

    # Try looking for it in the package's dldx list
    if (!is.null(dldx_list[[abbreviation]])) {
      out = dldx_list[[abbreviation]]
    } else {
      out = function(x, mu){
        # No gradient defined
        stop(
          "gradient with respect to x (\"dldx\") is not defined for the distribution '",
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
