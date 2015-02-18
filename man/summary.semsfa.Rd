\name{summary.semsfa}
\alias{summary.semsfa}
%\alias{summary}
\title{Summary for \code{semsfa} object}
\description{
Create and print summary results of a stochastic frontier model object returned by \code{semsfa()} with regard to the "CONDITIONAL EXPECTATION ESTIMATE" of the first step and to the "VARIANCE COMPONENTS ESTIMATE" of the compound error.
}
\usage{
\method{summary}{semsfa}(object, ...)
}
\arguments{
  \item{object}{
an \code{semsfa} object returned by semsfa()
}
  \item{\dots}{
further arguments to the summary method are currently ignored
}
}
\details{
Please note that if bootstrap is carried out the \eqn{t}-statistic is not reliable for testing the statistical significance of \eqn{\sigma} and \eqn{\lambda}, because these parameters are censored and cannot follow a \eqn{t}-distribution. 
We suggest to compare the BIC of the semiparametric estimated model with the base model. 
}
\value{
\code{summary.semsfa} returns a list of class \code{summary.semsfa} that is identical to an object returned by \code{semsfa()} with few modifications if bootstrap is carried out:
\item{b.t}{\eqn{t}-statistic given the bootstrapped standard errors for \eqn{\lambda} and \eqn{\sigma} (\code{b.se})}
\item{b.pv}{\eqn{p}-values of the \eqn{t}-statistic}
}

\author{
Giancarlo Ferrara and Francesco Vidoli
}

\note{
\code{summary} returns the same result if applied to an object created with \code{semsfa} or \code{efficiencies.semsfa}
}
\seealso{
\code{\link{semsfa}}, \code{\link{efficiencies.semsfa}}
}
\examples{ 
#generate data
set.seed(0)
n<-200

x<- runif(n, 1, 2)
fy<- 2+30*x-5*x^2
v<- rnorm(n, 0, 1)
u<- abs(rnorm(n,0,2.5))
#production frontier
y <- fy + v - u

dati<-data.frame(y,x)

#first-step: gam, second-step: fan (default)
#without bootstrap
o<-semsfa(y~s(x),dati,sem.method="gam")
summary(o)

# ... with bootstrap
o<-semsfa(y~s(x),dati,sem.method="gam",n.boot=100)
summary(o)
}