print.network = function(object){
  cat(
    ncol(object$x),
    "observed predictors and",
    ncol(object$par_skeleton$z),
    "latent ones\n\n"
  )

  cat(
    ncol(object$y),
    "response variables with",
    object$error_distribution$family[[2]],
    "errors\n"
  )

  cat("\nlayers:\n")
  for(i in 1:length(object$activators)){
    cat("  ", i, " ", object$activators[[i]]$name, "activation \n")
  }
}
