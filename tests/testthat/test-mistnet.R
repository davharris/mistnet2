context("Mistnet function")

n = 7    # Number of rows of data
n_x = 5  # Number of observed predictor variables
n_y = 7  # Number of response variables
bd = 13  # Number of coin flips per observation
n_z = 3  # Number of latent variables

x = matrix(rnorm(n * n_x), ncol = n_x)

y = gamlss.dist::rBI(
  n = n_y * n,
  mu = 0.5,
  bd = bd
)
dim(y) = c(n, n_y)


test_that("mistnet function runs and prints with no hidden layers", {
  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    activators = list(sigmoid_activator),
    priors = list(
      make_gamlss_distribution("NO", mu = 0, sigma = 1)
    ),
    n_hidden = NULL,
    error_distribution = "BI",
    fit = FALSE,
    bd = bd
  )

  expect_output(print(net), "7 observations\n\n5 observed predictors and 3 latent variables\n\nlayers:\n  1: sigmoid layer with 7 nodes\n     Binomial error distribution")
})



test_that("mistnet function runs and prints with one hidden layer", {
  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    activators = list(sigmoid_activator, sigmoid_activator),
    priors = list(
      make_gamlss_distribution("NO", mu = 0, sigma = 1),
      make_gamlss_distribution("NO", mu = 0, sigma = 1)
    ),
    n_hidden = c(5),
    error_distribution = "BI",
    fit = FALSE,
    bd = bd
  )

  expect_output(
    print(net),
    "7 observations\n\n5 observed predictors and 3 latent variables\n\nlayers:\n  1: sigmoid layer with 5 nodes\n  2: sigmoid layer with 7 nodes\n     Binomial error distribution"
  )
})



test_that("mistnet function runs and prints with two hidden layers", {
  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    activators = list(sigmoid_activator, elu_activator, sigmoid_activator),
    priors = list(
      make_gamlss_distribution("NO", mu = 0, sigma = 1),
      make_gamlss_distribution("NO", mu = 0, sigma = 1),
      make_gamlss_distribution("NO", mu = 0, sigma = 1)
    ),
    n_hidden = c(5, 7),
    error_distribution = "BI",
    fit = FALSE,
    bd = bd
  )

  expect_output(
    print(net),
    "7 observations\n\n5 observed predictors and 3 latent variables\n\nlayers:\n  1: sigmoid layer with 5 nodes\n  2: elu layer with 7 nodes\n  3: sigmoid layer with 7 nodes\n     Binomial error distribution"
  )
})

context("Mistnet: numeric gradients")

test_that("mistnet's gradients are numerically accurate (without priors)",{
  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    activators = list(sigmoid_activator, elu_activator, sigmoid_activator),
    priors = list(
      make_gamlss_distribution("IU", mu = 0),
      make_gamlss_distribution("IU", mu = 0),
      make_gamlss_distribution("IU", mu = 0)
    ),
    n_hidden = c(5, 7),
    error_distribution = "BI",
    fit = FALSE,
    bd = bd
  )

  numeric_grad = numDeriv::grad(
    func = function(x){
      sum(log_density(net, par = x, include_penalties = FALSE))
    },
    x = unlist(net$par_skeleton)
  )
  backprop_grad = unlist(backprop(net, par = unlist(net$par_skeleton)))
  names(backprop_grad) = NULL


  expect_equal(
    numeric_grad,
    backprop_grad,
    tolerance = 1E-5
  )
})
