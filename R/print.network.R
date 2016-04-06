#' @export
print.mistnet_network = function(x, ...){
  cat("A mistnet network object",
      "\n\nThe data set includes:\n  ",
      nrow(x$x), "observations\n  ",
      ncol(x$x),
      "observed predictors\n  ",
      ncol(x$y),
      "response variables with a",
      x$error_distribution$family[[2]],
      "error distribution",
      "\n\nThe model includes",
      ncol(x$par_list$z), "latent variables and the following layers:\n"
  )
  for (i in 1:length(x$activators)) {
    n_nodes = ncol(x$par_list$weights[[i]])
    cat(
      "   ",
      i,
      ": ",
      x$activators[[i]]$name,
      " layer with ",
      n_nodes,
      " nodes\n",
      sep = ""
    )
  }
}
