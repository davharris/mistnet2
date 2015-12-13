#' @import gamlss.dist
make_binomial_loss = function(n){
  structure(
    c(
      BI(),
      loss = function(x, mu){
        sum(-dBI(x = x, mu = mu, bd = n, log = TRUE))
      }
    ),
    class = c("loss", class(BI()))
  )
}

#' @import gamlss.dist
make_normal_loss = function(sigma){
  structure(
    c(
      NO(),
      loss = function(x, mu){
        -sum(dNO(x = x, mu = mu, sigma = sigma, log = TRUE))
      }
    ),
    class = c("loss", class(BI()))
  )
}
