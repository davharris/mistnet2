context("Activation functions")
x = matrix(rnorm(12), ncol = 3)

test_that("Activation functions' gradients are correct",{
  expect_equal(
    numDeriv::grad(elu_f$f, x),
    c(elu_f$grad(x))
  )

  expect_equal(
    numDeriv::grad(identity_f$f, x),
    c(identity_f$grad(x))
  )

  expect_equal(
    numDeriv::grad(relu_f$f, x),
    c(relu_f$grad(x))
  )

  expect_equal(
    numDeriv::grad(sigmoid_f$f, x),
    c(sigmoid_f$grad(x))
  )
})
