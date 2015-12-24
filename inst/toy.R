devtools::load_all(".") # Load the package in the working directory

n = 201  # Number of rows of data
n_x = 11 # Number of observed predictor variables
n_z = 5  # Number of latent variables
n_y = 19 # Number of response variables
bd = 501 # Number of coin flips per observation


# “True" parameters and data ----------------------------------------------

x = matrix(rnorm(n * n_x), ncol = n_x)
true_z = matrix(rnorm(n * n_z, sd = .5), ncol = n_z)
true_weights = matrix(rnorm((n_x + n_z) * n_y, sd = .5), ncol = n_y)
true_biases = rnorm(n_y, sd = 1)


y = gamlss.dist::rBI(
  n = n_y * n,
  mu = sigmoid_activator$f(cbind(x, true_z) %*% true_weights %plus%
                             true_biases + rnorm(n * n_y)),
  bd = bd
)
dim(y) = c(n, n_y)


# Initialize the network --------------------------------------------------

# This network has a single layer, so there's only one weight matrix, one set of
# biases, one activator object, etc.
network = list(
  x = x,
  y = y,
  par_list = list(
    z = matrix(rnorm(n * n_z, sd = .5), nrow = n, ncol = n_z),
    weights = list(
      matrix(rnorm((n_x + n_z) * n_y, sd = .5), nrow = n_x + n_z, ncol = n_y)
    ),
    biases = list(
      rnorm(n_y, sd = .5)
    )
  ),
  activators = list(sigmoid_activator),
  distribution = make_distribution("BI", bd = bd),
  priors = list(
    make_distribution("IU")
  )
)
class(network) = "network"


# Optimize ----------------------------------------------------------------

starttests = TRUE # Test my gradients before optimizing
o = optimx::optimx(
  par = unlist(network$par_list),
  fn = function(par){
    sum(log_density(network, par = par))
  },
  gr = function(par){
    unlist(backprop(network, par = par))
  },
  method = "L-BFGS-B",
  control = list(trace = 0, maximize = TRUE, starttests = starttests,
                 maxit = 1000),
  hessian = FALSE
)

estimates = relist(coef(o), network$par_list)



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
