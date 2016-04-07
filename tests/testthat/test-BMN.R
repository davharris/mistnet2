context("Markov networks")
test_that("Markov network pseudolikelihood is correct", {
  # This notation will follow Höfling and Tibshirani 2008.
  # See page 888

  x = matrix(rbinom(1000, size = 1, prob = .5), ncol = 20)

  n = nrow(x)
  p = ncol(x)

  theta_ss = rnorm(p)
  theta_st = matrix(0, p, p)

  # Fill in upper triangle of theta_st, then add to its transpose to symmetrize
  theta_st[upper.tri(theta_st)] = rnorm(choose(p, 2))
  theta_st = theta_st + t(theta_st)

  # Vector with one log-pseudo-likelihood for each site
  lpl = numeric(n)

  # Höfling and Tibshirani give the value for one row of x at a time, so we loop
  # over the rows.
  for (i in 1:n) {
    # this will store: x_s * x_t * theta_{st}
    xxtheta = matrix(0, p, p)
    for (s in 1:p) {
      # This will store: the log-normalizing constant
      psi_s = numeric(p)

      # this will store: sum (over s!=t) of (x_t * theta_{st}).
      # theta[s, s] is always zero, so we don't have to exclude the s==t case
      sum_xtheta = 0
      for (t in 1:p) {
        xxtheta = x[i, s] * x[i, t] * theta_st[s, t]
        sum_xtheta = sum_xtheta + x[i, t] * theta_st[s, t]
      }
      psi_s[s] = log(1 + exp(theta_ss[s] + sum(xxtheta)))
    }
    # Pseudolikelihood for this row of x
    lpl[i] = sum_xtheta + sum(psi_s)
  }
})
