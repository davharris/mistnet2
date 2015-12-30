context("Prediction and sampling")

test_that("feedforward_with_samples works",{
  `%plus%` = mistnet2:::`%plus%`

  n = 50
  n_x = 4
  n_y = 10
  n_z = 3

  x = matrix(rnorm(n * n_x), nrow = n, ncol = n_x)

  net = mistnet(
    x = x,
    y = matrix(0, nrow = n, ncol = n_y),
    n_z = n_z,
    layers = layer(elu_activator, n_y, weight_prior = make_distribution("NO", mu = 0, sigma = 1)),
    error_distribution = make_distribution("NO", mu = 0, sigma = 1),
    fit = FALSE
  )

  samples = replicate(
    10,
    matrix(draw_samples(net$z_prior, n = n * n_z), nrow = n, ncol = n_z),
    simplify = FALSE
  )

  states = mistnet2:::feedforward_with_samples(net, samples)

  # Manually feed forward for all teh samples and confirm that I get the same
  # thing both ways.
  predicted_states = lapply(
    samples,
    function(z) {
      inputs = list(cbind(x, z))
      pre_activations = list(
        inputs[[1]] %*% net$par_list$weights[[1]] %plus%
          net$par_list$biases[[1]]
      )
      outputs = list(net$activators[[1]]$f(pre_activations[[1]]))

      dimnames(outputs[[1]]) = dimnames(net$y)

      structure(
        list(
          inputs = inputs,
          pre_activations = pre_activations,
          outputs = outputs
        ),
        class = "network_state"
      )
    }
  )

  expect_equal(states, predicted_states)
})
