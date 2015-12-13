context("Activation functions")
x = matrix(rnorm(12), ncol = 3)

test_that("Activation functions' gradients are correct",{
  expect_equal(
    numDeriv::grad(elu_activator$f, x),
    c(elu_activator$grad(x))
  )

  expect_equal(
    numDeriv::grad(identity_activator$f, x),
    c(identity_activator$grad(x))
  )

  expect_equal(
    numDeriv::grad(relu_activator$f, x),
    c(relu_activator$grad(x))
  )

  expect_equal(
    numDeriv::grad(sigmoid_activator$f, x),
    c(sigmoid_activator$grad(x))
  )
})
