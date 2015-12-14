context("dldx: gradients with respect to x")

test_that("dldx works for NO",{
  dist = make_gamlss_distribution("NO", sigma = 3)

  expect_equal(
    dist$dldx(x = pi, mu = exp(1)),
    numDeriv::grad(
      function(x){dist$log_density(x = x, mu = exp(1))},
      x = pi
    )
  )
})
