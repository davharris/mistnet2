context("Activation functions")
x = matrix(rnorm(12), ncol = 3)

test_that("Activation functions' edge cases work",{
  # Sigmoid should never reach 0 or 1; these produce infinite gradients
  expect_equal(sigmoid_activator$f(-Inf), .Machine$double.eps)
  expect_equal(sigmoid_activator$f(Inf), 1 - .Machine$double.eps)
})

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
