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

  # Don't pollute the global environment with the net object
  rm(net, envir = .GlobalEnv)
})


test_that("`...` is passed through mistnet to optimx", {
  # See previous test for why I'm suppressing warnings,
  # capturing output, and using <<-
  suppressWarnings(
    capture.output(
      net <<- mistnet(
        x = x,
        y = y,
        n_z = n_z,
        layers = layer_list,
        error_distribution = make_distribution("BI", bd = bd),
        fit = TRUE,
        itnmax = 1,
        method = "Nelder-Mead",
        control = list(maximize = TRUE)
      )
    )
  )

  # If it uses Nelder-Mead like I asked, it won't evaluate any gradients
  expect_true(is.na(net$optimization_results$gevals))

  # If itnmax == 1, fevals should not be much more than length(unlist(par_list))
  expect_less_than(net$optimization_results$fevals,
                   2 * length(unlist(net$par_list))
  )

})
