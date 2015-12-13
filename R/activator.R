#' Activator objects and activation functions for nonlinearities and their gradients
#' in neural networks
#'
#' @format
#' An object of class \code{activator} of length 3, containing
#' functions relating to neural network nonlinearities:
#' \itemize{
#'    \item{\code{f}: The activation function (nonlinearity) itself}
#'    \item{\code{grad}: The activation function's gradient with respect to
#'        \code{x}}
#'    \item{\code{initialize_activatorinal_biases}: A function used internally to
#'        initialize the biases of a network's output layer}
#' }
#' @details
#' The following activators/activation functions are currently included:
#' \itemize{
#'    \item{elu}: "exponential linear unit" \code{f(x)=x} when x>0 and \code{f(x)=exp(x)-1} otherwise
#'    \item{identity}: the identity function
#'    \item{relu}: "rectified linear unit" \code{f(x)=x} when x>0 and \code{f(x)=0} otherwise
#'    \item{sigmoid}: sigmoid (logistic) function, 1/(1 + exp(-x))
#' }
#' @rdname activator

#' @rdname activator
#' @export elu_activator
elu_activator = structure(
  list(
    f = function(x){
      ifelse_matrix(x > 0, x, exp(x) - 1)
    },
    grad = function(x){
      ones = matrix(1, nrow(x), ncol(x))
      ifelse_matrix(x > 0, ones, exp(x))
    },
    initialize_activatorinal_biases = function(y){
      rep(0, ncol(y))
    }
  ),
  class = "activator"
)


#' @rdname activator
#' @export identity_activator
identity_activator = structure(
  list(
    f = identity,
    grad = function(x){
      matrix(1, nrow(x), ncol(x))
    },
    initialize_activatorinal_biases = function(y){
      colMeans(y)
    }
  ),
  class = "activator"
)



#' @rdname activator
#' @export relu_activator
relu_activator = structure(
  list(
    f = function(x){
      zeros = matrix(0, nrow(x), ncol(x))

      ifelse_matrix(x > 0, x, zeros)
    },
    grad = function(x){
      zeros = matrix(0, nrow(x), ncol(x))
      ones = matrix(1, nrow(x), ncol(x))

      ifelse_matrix(x > 0, ones, zeros)
    },
    initialize_activatorinal_biases = function(y){
      rep(0, ncol(y))
    }
  ),
  class = "activator"
)

#' @rdname activator
#' @export sigmoid_activator
sigmoid_activator = structure(
  list(
    f = function(x){
      # Sometimes slightly slower than plogis, but has a ceiling and floor
      # to avoid boundaries at 0 and 1.
      storage.mode(x) = "numeric"
      make.link("logit")$linkinv(x)
    },
    grad = function(x){
      storage.mode(x) = "numeric"
      make.link("logit")$mu.eta(x)
    },
    initialize_activatorinal_biases = function(y){
      # regularized by adding 1 success and 1 failure to keep
      # initializations finite
      out = qlogis((colSums(y) + 1) / (nrow(y) + 2))
      out
    }
  ),
  class = "activator"
)