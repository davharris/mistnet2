% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mistnet.R
\name{mistnet}
\alias{mistnet}
\title{Build and fit a neural network with random effects}
\usage{
mistnet(x, y, n_z, layers, error_distribution,
  z_prior = make_distribution("NO", mu = 0, sigma = 1), fit = TRUE,
  mistnet_optimizer = mistnet_fit_optimx, ...)
}
\arguments{
\item{x}{A numeric matrix of predictor variables}

\item{y}{A numeric or integer matrix of response variables}

\item{n_z, }{The number of latent random variables to include as predictors
alongside x}

\item{layers}{A list of \code{\link{layer}} objects. Note that \code{n_nodes}
in the final layer must match \code{ncol(y)}.}

\item{error_distribution}{An \code{\link{distribution}} object determining
the error distribution for the response variables y.}

\item{z_prior}{A \code{\link{distribution}} object (standard Gaussian by
default) determining the prior on the latent variables.}

\item{fit}{Logical. Should the model be fitted or should an untrained model
be returned? Defaults to \code{TRUE}.}

\item{mistnet_optimizer}{passed to \code{\link{mistnet_fit}}. By default,
models are fitted using \code{\link{mistnet_fit_optimx}} using
\code{method = "L-BFGS-B"}.}

\item{...}{Additional arguments to \code{\link{mistnet_fit}}}
}
\value{
An object of class \code{network} and subclass \code{mistnet_network}.
  This object will contain the original \code{x} and \code{y} matrices,
  a list of adjustable parameters (\code{par_list}), [[etc.]]
}
\description{
Build and fit a neural network with random effects
}
\examples{
set.seed(1)

# Load data from the `vegan` package
data(mite, mite.env, package = "vegan")

# x is a matrix of environmental predictors
x = scale(model.matrix(~., data = mite.env)[, -1])

# y is a matrix of abundances (counts) for 35 species of mites
y = as.matrix(mite)

# Fit a neural network with one hidden layer of 10 nodes and an elu
# activation function. The response variable has a Poisson distribution
# with a log link (exp_activator). The prior distributions for each layer
# are each standard normal distributions, and two latent variables are used.
net = mistnet(
   x = x,
   y = y,
   n_z = 2,
   layers = list(
     layer(
       activator = elu_activator,
       n_nodes = 10,
       weight_prior = make_distribution("NO", mu = 0, sigma = 1)
     ),
     layer(
       activator = exp_activator,
       n_nodes = ncol(y),
       weight_prior = make_distribution("NO", mu = 0, sigma = 1)
     )
   ),
   error_distribution = make_distribution("PO")
)

print(net)

# show the model's predictions for each layer
str(feedforward(net, par = unlist(net$par_list)))

# Calculate the log-likelihood for each observation under the fitted model
log_prob(net, par = unlist(net$par_list), include_penalties = FALSE)

# Include penalty terms from the prior to calculate the log-posterior instead
log_prob(net, par = unlist(net$par_list), include_penalties = TRUE)
}

