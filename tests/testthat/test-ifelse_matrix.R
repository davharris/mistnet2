context("ifelse_matrix")

x = matrix(rnorm(1E3), ncol = 10)

test_that("ifelse_matrix gives the same answer as ifelse",{
  # rectified linlear unit (RELU) nonlinearity
  expect_equal(
    ifelse_matrix(x>0, x, matrix(0, nrow(x), ncol(x))),
    ifelse(x>0, x, 0)
  )

  # a made-up trig function
  expect_equal(
    ifelse_matrix(x>1, sin(x), cos(x)),
    ifelse(x>1, sin(x), cos(x))
  )
})
