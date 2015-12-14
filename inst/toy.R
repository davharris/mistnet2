devtools::load_all(".")
library(gamlss.dist)
library(optimx)

n = 201
n_x = 11
n_z = 5
n_y = 19
bd = 501

# Parameters --------------------------------------------------------------

x = matrix(rnorm(n * n_x), ncol = n_x)
true_z = matrix(rnorm(n * n_z, sd = .5), ncol = n_z)
true_weights = matrix(rnorm((n_x + n_z) * n_y, sd = .5), ncol = n_y)
true_biases = rnorm(n_y, sd = 1)


y = rBI(
  n = n_y * n,
  mu = sigmoid_activator$f(cbind(x, true_z) %*% true_weights %plus% true_biases + rnorm(n * n_y)),
  bd = bd
)
dim(y) = c(n, n_y)


network = list(
  x = x,
  y = y,
  par_skeleton = list(
    z = true_z * 0 + rnorm(length(true_z), sd = .01),
    weights = list(
      true_weights * 0 + rnorm(length(true_weights), sd = .01)
    ),
    biases = list(
      true_biases * 0 + rnorm(length(true_biases), sd = .01)
    )
  ),
  activators = list(sigmoid_activator),
  error_distribution = make_gamlss_distribution("BI", bd = bd)
)
class(network) = "network"


# Optimize ----------------------------------------------------------------

starttests = TRUE
o = optimx(
  par = unlist(network$par_skeleton),
  fn = function(par){logLik.network(network, par = par)},
  gr = function(par){unlist(backprop(network, par = par))},
  method = "L-BFGS-B",
  control = list(trace = 0, maximize = TRUE, starttests = starttests, maxit = 1000),
  hessian = FALSE
)

estimates = relist(coef(o), network$par_skeleton)



# plotting ----------------------------------------------------------------

par(mfrow = c(1, 2))

plot(
  estimates$weights[[1]][1:n_x, ],
  c(true_weights[1:n_x, ]),
  main = "slopes",
  asp = 1
)
abline(0,1, col = 2)

plot(
  estimates$biases[[1]],
  c(true_biases),
  main = "biases",
  asp = 1
)
abline(0,1, col = 2)

par(mfrow = c(1, 1))
