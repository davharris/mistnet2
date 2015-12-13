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

make_normal_loss = function(sigma){
  structure(
    c(
      BI(),
      loss = function(x, mu){
        -sum(dBI(x = x, mu = mu, bd = n, log = TRUE))
      }
    ),
    class = c("loss", class(BI()))
  )
}
