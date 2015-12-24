context("layers")

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

# Test up to four layers
layer_list = list(
  layer(activator = sigmoid_activator,n_nodes = 11,
        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
  ),
  layer(activator = sigmoid_activator,n_nodes = 3,
        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
  ),
  layer(activator = sigmoid_activator,n_nodes = 4,
        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
  ),
  layer(activator = sigmoid_activator,n_nodes = n_y,
        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
  )
)



context("Mistnet: numeric gradients")

test_that("mistnet's gradients are numerically accurate",{

  for (i in length(layer_list):1) {

    # Note that i counts down and that i indicates the first layer of
    # layer_list to include.  So the first time through the loop has just the
    # last layer, and the final time through the loop has all the layers.
    layers = layer_list[i:length(layer_list)]

    net = mistnet(
      x = x,
      y = y,
      n_z = n_z,
      layers = layers,
      error_distribution = make_distribution("BI", bd = bd),
      fit = FALSE
    )

    # make sure the print function doesn't throw an error
    capture.output(print(net))


    # Make sure the gradients are correct numerically
    numeric_grad = numDeriv::grad(
      func = function(x){
        sum(log_density(net, par = x, include_penalties = TRUE))
      },
      x = unlist(net$par_list)
    )
    backprop_grad = unlist(backprop(net, par = unlist(net$par_list)))
    names(backprop_grad) = NULL


    expect_equal(
      numeric_grad,
      backprop_grad,
      tolerance = 1E-5
    )
  }
})


context("Mistnet: fit")

test_that("multi-layer mistnet_fit works", {
  # optimx throws unnecessary warnings and outputs into testthat's
  # results.  Suppress them, but save `net` in the global environment so
  # I can still run tests. Then remove it below.
  suppressWarnings(
    capture.output(
      net <<- mistnet(
        x = x,
        y = y,
        n_z = n_z,
        layers = layer_list,
        error_distribution = make_distribution("BI", bd = bd),
        fit = TRUE
      )
    )
  )


  # Excluding the penalty from log_density should be the same as using a flat
  # prior
  flat_prior_net = net
  flat_prior_net$weight_priors = list(
    make_distribution("IU", mu = -1),
    make_distribution("IU", mu = 0),
    make_distribution("IU", mu = 1)
  )

  expect_equal(
    log_density(net, par = unlist(net$par_list),
                include_penalties = FALSE),
    log_density(flat_prior_net, par = unlist(net$par_list),
                include_penalties = TRUE)
  )

  # Don't pollute the global environment with the net object
  rm(net, envir = .GlobalEnv)
})
