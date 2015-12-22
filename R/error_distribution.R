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
    # Call the function and pull out the abbreviation
    family_object = family_function()
    abbreviation = family_object$family[[1]]
  }else{
    if(is.character(family_function)){
      # Use the abbreviation to grab the function and call it
      abbreviation = family_function
      family_object = get(abbreviation, mode = "function")()
    }
  }

  family_parameters = family_object$parameters

  if("bd" %in% names(formals(z$dldm))){
    family_parameters$bd = NA
  }

  dots = list(...)

  for(param_name in names(family_parameters)){
    if(param_name %in% names(dots)){
      family_parameters[[param_name]] = dots[[param_name]]
    }else{
      if(param_name == "mu"){
        # mu doesn't always need to be included in `dots` because network error
        # distributions get mu values from the network's final layer instead
        family_parameters[[param_name]] = NA
      }else{
        stop(param_name, " is required for the `", abbreviation, "` distribution")
      }
    }
  }

  # Get the `dABB` and `rABB` functions, where ABB is the abbreviation.
  log_density = get(paste0("d", abbreviation), mode = "function")
  sample = get(paste0("r", abbreviation), mode = "function")

  structure(
    list(
      family = family_object$family,
      family_parameters = family_parameters,
      log_density = function(...){log_density(..., log= TRUE)},
      sample = function(...){sample(...)},
      dldm = expand_formals(family_object$dldm),
      dldd = expand_formals(family_object$dldd),
      dldv = expand_formals(family_object$dldv),
      dldt = expand_formals(family_object$dldt),
      dldx = expand_formals(get_dldx(family_object))
    ),
    class = "error_distribution"
  )
}

#' Get parameter values from a distribution object
#'
#' @param distribution a distribution object
#' @param adjusted_values a \code{list} of adjusted values (for adjustable
#' parameters)
#' @param ... additional arguments, which will override values contained within
#'    the \code{error_distribution} object or in the \code{adjusted_values}
get_values = function(distribution, adjusted_values, ...){

  values = list(...)

  for(param_name in names(distribution$family_parameters)){
    if(param_name %in% names(values)){
      # Do nothing: a value has already been provided
    }else{
      if(is.adjustable(distribution$family_parameters[[param_name]])){
        # Get the updated value from `adjusted_values`
        values[[param_name]] = adjusted_values[[param_name]]
      }else{
        # pull the value from the distribution object itself
        values[[param_name]] = distribution$family_parameters[[param_name]]
      }
    }
  }

  return(values)
}


get_dldx = function(family_object){
  abbreviation = family_object$family[[1]]

  # User-defined objects should contain dldx if needed
  out = family_object$dldx

  # gamlss-based objects won't contain dldx
  if(is.null(family_object$dldx)){

    # Try looking for it in the package's dldx list
    if(!is.null(dldx_list[[abbreviation]])){
      out = dldx_list[[abbreviation]]
    }else{
      out = function(x, mu){
        # No gradient defined
        stop(
          "gradient with respect to ",
          x,
          " (",
          dldx,
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
#' log-density are always zero. It's improper because its integral is not one
#' (and is not finite).
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

#' @export
#' @rdname IU
dIU = function(x, mu){
  stopifnot(log)
  rep(0, max(length(x), length(mu)))
}
