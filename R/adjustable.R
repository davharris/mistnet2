adjustable = function(x){
  structure(x, class = c("adjustable", class(x)))
}

is.adjustable = function(x){
  is(x, "adjustable")
}
