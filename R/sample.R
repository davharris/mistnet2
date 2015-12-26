#' @export
draw_samples = function(object, ...){
  UseMethod("draw_samples")
}

#' Sample random numbers from a probability distribution
#' @param distribution an \code{\link{distribution}} object
#' @param ... additional arguments passed to \code{\link{get_values}}
#' @export
draw_samples.distribution = function(object, ...){
  do.call(
    object$r,
    get_values(object, ...)
  )
}

