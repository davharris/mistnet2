#' @param n number of times to repeat each element
#' @param rep_rows logical. Should values be repeated across rows (TRUE) or across
#'    columns (FALSE)?
#' @rdname inflate
#' @export
inflatable = function(x, rep_rows, n){
  assert_that(is.null(dim(x)))

  structure(x, class = c("inflatable", class(x)), n = n, rep_rows = rep_rows)
}

#' inflate an \code{inflatable} vector
#'
#' description
#'
#' @param x a vector
#'
#' @export inflate
inflate = function(x){
  UseMethod("inflate")
}

#' @export
inflate.default = identity

#' @export
inflate.inflatable = function(x){
  n = attr(x, "n")

  out = kronecker(matrix(rep(1, n), nrow = 1), x)

  if (attr(x, "rep_rows")) {
    return(out)
  } else {
    return(t(out))
  }
}



