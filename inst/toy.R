library(gamlss.dist)
library(optimx)

n = 501
n_x = 9
n_z = 5
n_y = 23
bd = 51

# functions ---------------------------------------------------------------

nonlinearity = function(x){
  # Sometimes slightly slower than plogis, but has a floor and ceiling to avoid
  # hitting 0 or 1
  storage.mode(x) = "numeric"
  make.link("logit")$linkinv(x)
}
nonlinearity_grad = make.link("logit")$mu.eta

error_dist = BI()
log_error_density = function(mu){
  dBI(x = y, mu = mu, bd = bd, log = TRUE)
}
density_grad = function(mu){
  error_dist$dldm(y = y, mu = mu, bd = bd)
}


activation = function(z, b){
  cbind(x, z) %*% b
}

loglik = function(p){
  z = relist(p, plist)$z
  b = relist(p, plist)$b

  sum(log_error_density(nonlinearity(activation(z, b))))
}
grad = function(p){
  z = relist(p, plist)$z
  b = relist(p, plist)$b
  act = activation(z, b)
  mu = nonlinearity(act)


  coef_grad = nonlinearity_grad(act) * density_grad(mu)
  input_grad = tcrossprod(coef_grad, b)


  c(
    z = input_grad[ , -(1:ncol(x))],
    b = crossprod(cbind(x, z), coef_grad)
  )
}


# Parameters --------------------------------------------------------------

x = matrix(rnorm(n * n_x), ncol = n_x)
true_z = matrix(rnorm(n * n_z, sd = .25), ncol = n_z)
true_b = matrix(rnorm((n_x + n_z) * n_y, sd = .25), ncol = n_y)



plist = list(
  z = true_z * 0 + rnorm(length(true_z), sd = .01),
  b = true_b * 0 + rnorm(length(true_b), sd = .01)
)
start_p = unlist(plist)
is_slope = grepl("^b", names(start_p))
slope_is_for_observed = c(col(true_b) <= n_x)
# finite_grads = numDeriv::grad(loglik, start_p)


y = rBI(
  n = n_y * n,
  mu = nonlinearity(activation(true_z, true_b) + rnorm(n * n_y)),
  bd = bd
)
dim(y) = c(n, n_y)



# Optimize ----------------------------------------------------------------

starttests = FALSE
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
  estimates$b[1:n_x, ],
  c(true_b[1:n_x, ])
)
