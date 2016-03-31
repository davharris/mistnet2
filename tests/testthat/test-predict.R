context("Prediction and sampling")

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
  error_distribution = make_distribution("NO", sigma = 1),
  fit = FALSE
)



test_that("feedforward_with_samples works",{
  `%plus%` = mistnet2:::`%plus%`

  samples = replicate(
    10,
    matrix(draw_samples(net$z_prior, n = n * n_z), nrow = n, ncol = n_z),
    simplify = FALSE
  )

  states = mistnet2:::feedforward_with_samples(net, samples)

  # Manually feed forward for all the samples and confirm that I get the same
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

test_that("predict.mistnet_network works", {
  n_samples = 5
  newdata = matrix(rnorm(n * n_x), nrow = n, ncol = n_x)

  # create a network with newdata
  newnet = net
  newnet$x = newdata

  # Generate samples and feed them forward manually through newnet
  set.seed(1)
  z_samples = replicate(
    n_samples,
    {
      matrix(
        draw_samples(net$z_prior, n = nrow(newdata) * n_z),
        nrow = nrow(newdata),
        ncol = n_z
      )
    },
    simplify = FALSE
  )
  predicted_manual = mistnet2:::feedforward_with_samples(newnet, z_samples)

  # Compare with `predict` output with full_state when full_state is TRUE
  set.seed(1)
  predicted = predict(object = net, newdata = newdata, n_samples = n_samples,
                      full_state = TRUE)

  expect_identical(predicted_manual, predicted)

  # Make sure the output array is good if full_state is FALSE
  set.seed(1)
  predicted = predict(object = net, newdata = newdata, n_samples = n_samples,
                      full_state = FALSE)
  for (i in 1:n_samples) {
    expect_equal(
      predicted_manual[[i]]$outputs[[length(predicted_manual[[i]]$outputs)]],
      predicted[ , , i]
    )
  }

})
