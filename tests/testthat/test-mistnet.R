context("Mistnet: layers")

set.seed(1)

n = 5    # Number of rows of data
n_x = 3  # Number of observed predictor variables
n_y = 2  # Number of response variables
bd = 13  # Number of coin flips per observation
n_z = 2  # Number of latent variables

x = matrix(rnorm(n * n_x), ncol = n_x)

y = gamlss.dist::rBI(
  n = n_y * n,
  mu = 0.5,
  bd = bd
)
dim(y) = c(n, n_y)

test_that("layer function's optional arguments work", {
  example_layer = layer(
    activator = sigmoid_activator,
    n_nodes = ncol(y),
    weight_prior = make_distribution("NO", mu = 0, sigma = 1),
    weights = matrix(pi, nrow = n_x + n_z, ncol = ncol(y)),
    biases = rep(exp(2), ncol(y))
  )

  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    layers = list(example_layer),
    error_distribution = make_distribution("BI", bd = bd),
    fit = FALSE
  )

  expect_true(all(net$par_list$weights[[1]] == pi))
  expect_true(all(net$par_list$biases[[1]] == exp(2)))
})

context("Mistnet: different arguments to par")

test_that("build_par_list works on networks", {
  net = mistnet(
    x = x,
    y = y,
    n_z = n_z,
    layers = list(
      layer(
        activator = sigmoid_activator,
        n_nodes = ncol(y),
        weight_prior = make_distribution("NO", mu = 0, sigma = 1)
      )
    ),
    error_distribution = make_distribution("BI", bd = bd),
    fit = FALSE
  )


  # Compare missing with list in `feedforward`
  expect_identical(
    feedforward(network = net),
    feedforward(network = net, par = net$par_list)
  )

  # Compare missing with unlisted() in `feedforward`
  expect_identical(
    backprop(network = net),
    backprop(network = net, par = unlist(net$par_list))
  )
})


# Test up to four layers
layer_list = list(
  layer(activator = sigmoid_activator,n_nodes = 5,
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

    evaluate_promise({
      net = mistnet(
        x = x,
        y = y,
        n_z = n_z,
        layers = layers,
        error_distribution = make_distribution("BB", bd = bd, sigma = adjustable(10)),
        fit = FALSE
      )
    })

    # make sure the print function doesn't throw an error
    capture.output(print(net))


    # Make sure the gradients are correct numerically
    numeric_grad = numDeriv::grad(
      func = function(x){
        sum(log_prob(net, par = x, include_penalties = TRUE))
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
  # results.  Capture them with evaluate_promise.
  evaluate_promise({
    net = mistnet(
      x = x,
      y = y,
      n_z = n_z,
      layers = layer_list,
      error_distribution = make_distribution("BI", bd = bd),
      fit = TRUE,
      control = list(maximize = TRUE, starttests = TRUE)
    )
  })

  # Excluding the penalty from log_prob should be the same as using a flat
  # prior on weights and z
  flat_prior_net = net
  flat_prior_net$weight_priors = lapply(
    1:length(net$weight_priors),
    function(i){
      make_distribution("IU", mu = 0)
    }
  )
  flat_prior_net$z_prior = make_distribution("IU", mu = 0)


  expect_equal(
    log_prob(net,
             par = unlist(net$par_list),
             include_penalties = FALSE),
    log_prob(flat_prior_net,
             par = unlist(net$par_list),
             include_penalties = TRUE)
  )
})

test_that("`...` is passed through mistnet to optimx", {
  evaluate_promise({
    net = mistnet(
      x = x,
      y = y,
      n_z = n_z,
      layers = layer_list,
      error_distribution = make_distribution("BI", bd = bd),
      fit = TRUE,
      itnmax = 1,
      method = "Nelder-Mead",
      control = list(maximize = TRUE, starttests = TRUE)
    )
  })

  # If it uses Nelder-Mead like I asked, it won't evaluate any gradients
  expect_true(is.na(net$optimization_results$gevals))

  # If itnmax == 1, fevals should not be much more than length(unlist(par_list))
  expect_less_than(net$optimization_results$fevals,
                   2 * length(unlist(net$par_list))
  )

})


context("Mistnet: convex parameter recovery")
# With just one layer, neural networks have convex objective functions,
# and we can expect a functioning network to recover the "true" parameters
# instead of getting stuck in a local optimum (apart from identifiability
# issues with the latent variables).

# Clear the workspace for this context and start over
rm(list = ls())

test_that("single-layer network recovers 'true' parameters", {
  n = 500   # Number of rows of data
  n_x = 3   # Number of observed predictor variables
  n_y = 20   # Number of response variables
  n_z = 2   # Number of latent variables

  # "true" weights used to produce the new y.
  # Used below to confirm the model fits
  # correctly in a one-layer network
  true_weights = matrix(
    rnorm((n_x + n_z) * n_y),
    ncol = n_y
  )

  true_x = matrix(
    rnorm(n_x * n),
    ncol = n_x
  )

  true_z = matrix(
    rnorm(n_z * n),
    ncol = n_z
  )

  true_mu = cbind(true_x, true_z) %*% true_weights
  true_sigma = .1

  true_y = rnorm(
    n = length(true_mu),
    mean = true_mu,
    sd = true_sigma
  )
  dim(true_y) = dim(true_mu)

  net = mistnet(
    x = true_x,
    y = true_y,
    n_z = n_z,
    layers = list(
      layer(activator = identity_activator,
            n_nodes = ncol(true_y),
            weight_prior = make_distribution("NO", mu = 0, sigma = 1)
      )
    ),
    error_distribution = make_distribution("NO", sigma = .1),
    z_prior = make_distribution("NO", mu = 0, sigma = 1),
    fit = FALSE
  )

  evaluate_promise({
    net = mistnet_fit(
      net,
      control = list(maximize = TRUE, starttests = FALSE)
    )
  })

  fitted_weights = net$par_list$weights[[1]]

  # R-squared for the weights associated with all columns of x
  # This version of R2 doesn't include an intercept or non-unit slope
  diff = (true_weights[1:n_x, ] - fitted_weights[1:n_x, ])
  R2 = 1 - sum(diff^2) / sum(true_weights[1:n_x, ]^2)
  expect_more_than(R2, .95)


  # Identify rows of weights that deal with z
  is_z = seq(1, n_x + n_z) > n_x

  # Linear models for the z-related weights.  These linear models just confirm
  # that the z-weights are in the correct linear subspace, since the z variables
  # are interchangeable in the model.
  z_weight_summaries = summary(
    lm(t(true_weights[is_z, ]) ~ t(fitted_weights[is_z, ]))
  )
  lapply(
    z_weight_summaries,
    function(summary){
      expect_more_than(summary$r.squared, .95)
    }
  )

  # list of summarized linear models for the latent variables themselves
  z_summaries = summary(lm(true_z ~ net$par_list$z))
  lapply(
    z_summaries,
    function(summary){
      expect_more_than(summary$r.squared, .95)
    }
  )
})
