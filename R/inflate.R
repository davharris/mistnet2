#' "inflate" a vector by repeating rows or columns
#'
#' A vector can be tagged with the attribute \code{inflatable} by
#' \code{make_inflatable}. This is useful when a vector of adjustable parameters
#' needs to be re-used across the rows or columns of a matrix (e.g. if each row
#' of an error distribution has its own standard deviation).
#'
#'
#' \code{inflatable} vectors can be \code{inflate}d to repeat their
#' elements across the rows or columns in a newly-formed matrix, but are
#' otherwise unchanged. Calling \code{inflate()} on a non-inflatable vector
#' will leave the vector unchanged. The derivatives of \code{inflatable} vectors
#' are automatically handled separatly by \code{\link{backprop}()}
#' @param x a vector
#'
#' @export inflate
inflate = function(x){
  UseMethod("inflate")
}

#' @rdname inflate
#' @export
inflate.default = identity

#' @rdname inflate
#' @export
inflate.inflatable = function(x){
  n = attr(x, "n")

  out = matrix(x, nrow = length(x), ncol = n)

  if (attr(x, "by") == "col") {
    out = t(out)
  }

  out
}

#' @param n number of times to repeat each element
#' @param by one of \code{"row"} or \code{"col"}. Should elements be repeated
#'    across rows or across columns?
#' @rdname inflate
#' @export
make_inflatable = function(x, by, n){
  assert_that(is.null(dim(x)))

  structure(x, class = c("inflatable", class(x)), n = n, by = by)
}


deflate = function(x, by){
  switch(
    by,
    row = rowSums(x),
    col = colSums(x)
  )
}
