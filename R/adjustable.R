#' @export
adjustable = function(x){
  structure(x, class = c("adjustable", class(x)))
}

#' @export
is.adjustable = function(x){
  is(x, "adjustable")
}
