#' Flag a distribution parameter for optimization
#' @param x An argument to a \code{\link{make_distribution}}
#' @export
#' @examples
#' # This distribution object is marked as having a fixed standard deviation
#' # and a mean that should be optimized
#' dist = make_distribution("NO", mu = adjustable(0), sigma = 1)
adjustable = function(x){
  structure(x, class = c("adjustable", class(x)))
}


get_adjustables = function(x){
  purrr::keep(x$family_parameters, is, "adjustable")
}
