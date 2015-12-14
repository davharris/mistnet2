context("make_gamlss_distribution")

test_that("The bd argument is passed properly",{
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
})
