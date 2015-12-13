devtools::load_all(".")
library(gamlss.dist)
library(optimx)

n = 201
n_x = 11
n_z = 5
n_y = 19
bd = 501

# functions ---------------------------------------------------------------

activator = sigmoid_activator

error_dist = BI()
log_error_density = function(mu){
  dBI(x = y, mu = mu, bd = bd, log = TRUE)
}
density_grad = function(mu){
  error_dist$dldm(y = y, mu = mu, bd = bd)
}


pre_activation = function(z, w, b){
  (cbind(x, z) %*% w) %plus% b
}

loglik = function(p){
  z = relist(p, plist)$z
  w = relist(p, plist)$w
  b = relist(p, plist)$b

  sum(log_error_density(activator$f(pre_activation(z, w, b))))
}
grad = function(p){
  z = relist(p, plist)$z
  w = relist(p, plist)$w
  b = relist(p, plist)$b

  pre_act = pre_activation(z, w, b)
  output = activator$f(pre_act)


  grad_from_above = activator$grad(pre_act) * density_grad(output)
  input_grad = tcrossprod(grad_from_above, w)

  c(
    z = input_grad[ , -(1:ncol(x))],
    w = crossprod(cbind(x, z), grad_from_above),
    b = colSums(grad_from_above)
  )
}


# Parameters --------------------------------------------------------------

x = matrix(rnorm(n * n_x), ncol = n_x)
true_z = matrix(rnorm(n * n_z, sd = .5), ncol = n_z)
true_w = matrix(rnorm((n_x + n_z) * n_y, sd = .5), ncol = n_y)
true_b = rnorm(n_y, sd = 1)



plist = list(
  z = true_z * 0 + rnorm(length(true_z), sd = .01),
  w = true_w * 0 + rnorm(length(true_w), sd = .01),
  b = true_b * 0 + rnorm(length(true_b), sd = .01)
)
start_p = unlist(plist)
is_slope = grepl("^w", names(start_p))
slope_is_for_observed = c(col(true_w) <= n_x)


y = rBI(
  n = n_y * n,
  mu = activator$f(pre_activation(true_z, true_w, true_b) + rnorm(n * n_y)),
  bd = bd
)
dim(y) = c(n, n_y)



# Optimize ----------------------------------------------------------------

starttests = TRUE
o = optimx(
  par = start_p,
  fn = loglik,
  gr = grad,
  method = "L-BFGS-B",
  control = list(trace = 0, maximize = TRUE, starttests = starttests, maxit = 1000),
  hessian = FALSE
)

estimates = relist(coef(o), plist)

par(mfrow = c(1, 2))

plot(
  estimates$w[1:n_x, ],
  c(true_w[1:n_x, ]),
  main = "slopes",
  asp = 1
)
abline(0,1, col = 2)

plot(
  estimates$b,
  c(true_b),
  main = "biases",
  asp = 1
)
abline(0,1, col = 2)

par(mfrow = c(1, 1))
