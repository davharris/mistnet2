devtools::load_all(".")
library(gamlss.dist)
library(optimx)

n = 201
n_x = 9
n_z = 5
n_y = 17
bd = 51

# functions ---------------------------------------------------------------

activator = sigmoid_activator
1
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

  act = pre_activation(z, w, b)
  mu = activator$f(act)


  coef_grad = activator$grad(act) * density_grad(mu)
  input_grad = tcrossprod(coef_grad, w)

  c(
    z = input_grad[ , -(1:ncol(x))],
    w = crossprod(cbind(x, z), coef_grad),
    b = colSums(coef_grad)
  )
}


# Parameters --------------------------------------------------------------

x = matrix(rnorm(n * n_x), ncol = n_x)
true_z = matrix(rnorm(n * n_z, sd = .25), ncol = n_z)
true_w = matrix(rnorm((n_x + n_z) * n_y, sd = .25), ncol = n_y)
true_b = rnorm(n_y, sd = .25)



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

plot(
  estimates$w[1:n_x, ],
  c(true_w[1:n_x, ])
)
