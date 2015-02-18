\name{fan}
\alias{fan}
\title{Pseudolikelihood estimator of the \eqn{\lambda} parameter}
\description{
Pseudolikelihood estimator of the \eqn{\lambda} parameter
}
\usage{
fan(lambda_fan, resp, Ey, ineffD)
}
\arguments{
  \item{lambda_fan}{
the \eqn{\lambda=\sigma_u/\sigma_v} parameter to be estimated
}
  \item{resp}{
the single response variable Y observed
}
  \item{Ey}{
the conditional expectation estimate obtained in the first step of the algorithm
}
  \item{ineffD}{
logical: TRUE for estimating a production function, FALSE for estimating a cost function; this is done for usage compatibility with frontier package}
}
\value{
Estimated \eqn{\lambda} parameter
}
\references{
Fan, Y., Li, Q., Weersink, A., 1996. Semiparametric estimation of stochastic production frontier models. Journal of Business & Economic Statistics 14:460-468
}
\author{
Giancarlo Ferrara and Francesco Vidoli
}
\note{
Internal usage only
}