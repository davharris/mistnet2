context("Distributions")

test_that("Distribution parameters are checked",{
  expect_error(
    make_gamlss_distribution("NO"),
    "sigma is required for the `NO` distribution"
  )

  expect_error(
    make_gamlss_distribution("NO", mu = 3, sigma = 2),
    "mu should not be given when making an error_distribution"
  )
})

test_that("Arguments are passed properly",{
  # Binomial distribution

  distribution = make_gamlss_distribution("BI", bd = 5)

  # By default, the gamlss binomial log density dBI sets bd to 1.
  # If these are equal, it means bd=5 from above is being passed properly
  expect_equal(
    distribution$log_density(3, .2),
    dbinom(3, size = 5, prob = .2, log = TRUE)
  )

  # Similarly, if bd=5 and mu = 1 are passed, all the samples should equal 5
  expect_true(
    all(distribution$sample(300, mu = 1) == 5)
  )

  expect_equal(
    distribution$dldm(0:5, .9),
    BI()$dldm(0:5, .9, 5)
  )


  # Zero-inflated negative binomial distribution with mu, sigma, nu

  distribution = make_gamlss_distribution("ZINBI", sigma = 3, nu = 5)

  # gradient with respect to nu
  expect_equal(
    distribution$dldv(x = 3, mu = 4),
    ZINBI()$dldv(y = 3, mu = 4, sigma = 3, nu = 5)
  )
})


test_that("meaningful errors are thrown when dldx isn't available",{
  dist = make_gamlss_distribution("BI", bd = 3)

  expect_error(
    dist$dldx(x = pi, mu = exp(1)),
    " x .*(dldx).*'BI'"
  )
})

test_that("dldx works for distributions that have it", {
  dist = make_gamlss_distribution("NO", sigma = 3)

  expect_equal(
    dist$dldx(x = pi, mu = exp(1)),
    numDeriv::grad(
      function(x){dist$log_density(x = x, mu = exp(1))},
      x = pi
    )
  )
})

