library(gamlss.dist)
library(optimx)
n = 5000
n_x = 20
n_z = 10
n_y = 200
bd = 500


x = matrix(rnorm(n * n_x), ncol = n_x)
true_z = matrix(rnorm(n * n_z, sd = .25), ncol = n_z)
true_b = matrix(rnorm((n_x + n_z) * n_y, sd = .25), ncol = n_y)

nonlinearity = function(x){
  # Sometimes slightly slower than plogis, but has a floor and ceiling to avoid
  # hitting 0 or 1
  storage.mode(x) = "numeric"
  make.link("logit")$linkinv(x)
}
nonlinearity_grad = make.link("logit")$mu.eta
#nonlinearity = identity
#nonlinearity_grad = function(x) rep(1, length(x))

y = rBI(
  n = n_y * n,
  mu = nonlinearity(cbind(x, true_z) %*% true_b + rnorm(n * n_y)),
  bd = bd
)
dim(y) = c(n, n_y)

plist = list(
  z = true_z * 0 + rnorm(length(true_z), sd = .01),
  b = true_b * 0 + rnorm(length(true_b), sd = .01)
)


error_dist = BI()
log_error_density = function(mu){
  dBI(x = y, mu = mu, bd = bd, log = TRUE)
}
density_grad = function(mu){
  error_dist$dldm(y = y, mu = mu, bd = bd)
}


# error_dist = NO()
# log_error_density = function(mu){
#   dNO(x = y, mu = mu, log = TRUE)
# }
# density_grad = function(mu){
#   error_dist$dldm(y = y, mu = mu, sigma = 1)
# }


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


  outgoing_grad = nonlinearity_grad(act) * density_grad(mu)
  incoming_grad = tcrossprod(
    outgoing_grad,
    b
  )

  c(
    z = incoming_grad[ , -(1:ncol(x))],
    b = mistnet:::matrixMultiplyGrad(
      ncol(y),
      outgoing_grad,
      cbind(x, z)
    )
  )
}


start_p = unlist(plist)
is_slope = grepl("^b", names(start_p))
slope_is_for_observed = c(col(true_b) <= n_x)
# finite_grads = numDeriv::grad(loglik, start_p)



starttests = FALSE
o = optimx(
  par = start_p,
  fn = loglik,
  gr = grad,
  method = "L-BFGS-B",
  control = list(trace = 1, REPORT = 1, fnscale = -1, starttests = starttests),
  hessian = FALSE
)

estimates = relist(coef(o), plist)

plot(
  estimates$b[1:n_x, ],
  c(true_b[1:n_x, ])
)
