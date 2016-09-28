# base::unlist is slooooow when use.names is TRUE (the default)
# see https://www.r-bloggers.com/speed-trick-unlist-use-namesfalse-is-heaps-faster/
unlist = function(x){
  base::unlist(x, recursive = TRUE, use.names = FALSE)
}
