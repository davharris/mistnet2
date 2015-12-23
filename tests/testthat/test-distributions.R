context("Distributions")

test_that("make_gamlss_distribution accepts functions", {
  dist = make_gamlss_distribution(gamlss.dist::NO, sigma = 2)
})


test_that("make_gamlss_distribution rejects family objects", {
  expect_error(
    make_gamlss_distribution(NO(), sigma = 2),
    "not the object itself"
  )
})

test_that("Distribution parameters are checked",{
  expect_error(
    make_gamlss_distribution("NO"),
    "sigma is required for the `NO` distribution"
  )

  expect_error(
    make_gamlss_distribution("BI"),
    "bd is required for the `BI` distribution"
  )
})

test_that("Arguments are passed properly",{
  # Binomial distribution

  distribution = make_gamlss_distribution("BI", bd = 5)

  # By default, the gamlss binomial log density dBI sets bd to 1.
  # If these are equal, it means bd=5 from above is being passed properly
  expect_equal(
    log_density(distribution, x = 3, mu = 0.2),
    dbinom(3, size = 5, prob = .2, log = TRUE)
  )

  # Similarly, if bd=5 and mu = 1 are passed, all the samples should equal 5
  expect_true(
    all.equal(random_sample(distribution, n = 300, mu = 1), rep(5, 300))
  )

  # Gradient with respect to mu
  expect_equal(
    grad(distribution, "mu", y = 0:5, mu = 0.9),
    BI()$dldm(0:5, .9, 5)
  )


  # Zero-inflated negative binomial distribution with mu, sigma, nu

  distribution = make_gamlss_distribution("ZINBI", sigma = 3, nu = 5)

  # gradient with respect to nu
  expect_equal(
    grad(distribution, "nu", y = 3, mu = 4),
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
    grad(dist, "x", x = pi, mu = exp(1)),
    numDeriv::grad(
      function(x){log_density(dist, x = x, mu = exp(1))},
      x = pi
    )
  )
})


test_that("new distributions can be created", {
  EXAMPLE = function(){
    structure(
      list(
        parameters = list(mu = TRUE, sigma = TRUE),
        family = "EXAMPLE",
        dldx = function(x, mu, ...){
          1234
        },
        dldm = function(x, mu, ...){
          9876
        },
        dldd = function(x, mu, ...){
          3333
        }
      ),
      class = "family"
    )
  }

  dEXAMPLE = function(x, mu, sigma, log){
    8765
  }
  rEXAMPLE = function(x, mu, sigma, log){
    stop("random sampler for EXAMPLE distribution not defined")
  }
  attach(list(dEXAMPLE = dEXAMPLE))
  attach(list(rEXAMPLE = rEXAMPLE))

  dist = make_gamlss_distribution(EXAMPLE, sigma = 4)

  expect_equal(
    dist$dldx(1, 1),
    1234
  )
  expect_equal(
    dist$dldm(1, 1),
    9876
  )
  expect_equal(
    dist$dldd(1, 1),
    3333
  )

  expect_equal(
    log_density(dist, x = 1, mu = 1),
    8765
  )

})


test_that("Improper uniform distribution works", {
  dist = make_gamlss_distribution("IU")

  expect_equal(
    log_density(dist, 1:100, 1:100),
    rep(0, 100)
  )

  expect_equal(
    dist$dldm(y = 1:10, mu = 1:10),
    rep(0, 10)
  )

  expect_equal(
    dist$dldx(x = 1:10, mu = 1),
    rep(0, 10)
  )
})
