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
    error_distribution = make_gamlss_distribution("BI", bd = bd),
    fit = FALSE
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
    error_distribution = make_gamlss_distribution("BI", bd = bd),
    fit = FALSE
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
    error_distribution = make_gamlss_distribution("BI", bd = bd),
    fit = FALSE
  )

  expect_output(
    print(net),
    "7 observations\n\n5 observed predictors and 3 latent variables\n\nlayers:\n  1: sigmoid layer with 5 nodes\n  2: elu layer with 7 nodes\n  3: sigmoid layer with 7 nodes\n     Binomial error distribution"
  )
})

context("Mistnet: numeric gradients")

test_that("mistnet's gradients are numerically accurate",{
  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    activators = list(sigmoid_activator, elu_activator, sigmoid_activator),
    priors = list(
      make_gamlss_distribution("NO", mu = -1, sigma = 8),
      make_gamlss_distribution("NO", mu = 0, sigma = 3),
      make_gamlss_distribution("NO", mu = 1, sigma = 2)
    ),
    n_hidden = c(5, 7),
    error_distribution = make_gamlss_distribution("BI", bd = bd),
    fit = FALSE
  )

  numeric_grad = numDeriv::grad(
    func = function(x){
      sum(log_density(net, par = x, include_penalties = TRUE))
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


context("Mistnet: fit")

test_that("mistnet_fit runs with three layers", {
  # optimx throws unnecessary warnings and outputs into testthat's
  # results.  Suppress them, but save `net` in the global environment so
  # I can still run tests. Then remove it below.
  suppressWarnings(
    capture.output(
      net <<- mistnet(
        x = x,
        y = y,
        n_z = n_z,
        activators = list(sigmoid_activator, elu_activator, sigmoid_activator),
        priors = list(
          make_gamlss_distribution("NO", mu = -1, sigma = 8),
          make_gamlss_distribution("NO", mu = 0, sigma = 3),
          make_gamlss_distribution("NO", mu = 1, sigma = 2)
        ),
        n_hidden = c(5, 7),
        error_distribution = make_gamlss_distribution("BI", bd = bd),
        fit = TRUE
      )
    )
  )


  # Excluding the penalty from log_density should be the same as using a flat
  # prior
  flat_prior_net = net
  flat_prior_net$priors = list(
    make_gamlss_distribution("IU", mu = -1),
    make_gamlss_distribution("IU", mu = 0),
    make_gamlss_distribution("IU", mu = 1)
  )

  expect_equal(
    log_density(net, par = unlist(net$par_skeleton),
                include_penalties = FALSE),
    log_density(flat_prior_net, par = unlist(net$par_skeleton),
                include_penalties = TRUE)
  )

  # Don't pollute the global environment with the net object
  rm(net, envir = .GlobalEnv)
})
