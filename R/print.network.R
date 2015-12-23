print.network = function(object){
  cat(nrow(object$x), "observations\n\n")

  cat(
    ncol(object$x),
    "observed predictors and",
    ncol(object$par_skeleton$z),
    "latent variables\n\n"
  )

  cat("layers:\n")
  for(i in 1:length(object$activators)){
    n_nodes = ncol(object$par_skeleton$weights[[i]])
    cat(
      "  ",
      i,
      ": ",
      object$activators[[i]]$name,
      " layer with ",
      n_nodes,
      " nodes\n",
      sep = ""
    )
  }
  cat("    ", object$error_distribution$family[[2]], "error distribution")
}
