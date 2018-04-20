\name{efficiencies.semsfa}
\alias{efficiencies.semsfa}
\title{Prediction of the individual efficiency score}
\description{
This function calculates and returns efficiency estimates from semiparametric stochastic frontier models estimated with \code{semsfa()}.
}
\usage{
efficiencies.semsfa(semobj, log.output = TRUE, ...)
}
\arguments{
  \item{semobj}{
a stochastic frontier model object returned by \code{semsfa()}
}
  \item{log.output}{
logical. Is the dependent variable logged?
}
  \item{\dots}{
further arguments to the summary method are currently ignored
}
}
\details{
The estimation of the individual efficiency score for a particular point \eqn{(x,y)} on a production frontier might be obtained from the Jondrow et al. (1982) procedure. Defining:
\deqn{
 \sigma^2=\sigma_u^2+\sigma_v^2, u_{*}(x) = -\sigma_u^2 \epsilon/\sigma^2, \sigma_{*}^2=\sigma_u^2 \sigma_v^2/\sigma^2
}
it can be shown that:
\deqn{
u|\epsilon ~ N^+(\mu_{*}(x),\sigma_{*}^{2}(x)).
}
We can use this distribution to obtain point previsions of \eqn{u} trought the mean of the conditional distribution:
\deqn{
E(u|\epsilon)=\mu_{*} + \sigma_{*} f(-\mu_{*}/\sigma_{*})/(1-F(\mu_{*}/\sigma_{*}))
}
where \eqn{f} and \eqn{F} represent the standard Normal density and cumulative distribution function, respectively; alternative formulas for cost frontier models are easy to get (please see Kumbhakar and Lovell, 2000).

If the response variable is measured in logs, a point estimate of the efficiency is then provided by \eqn{\exp(-u) \in (0,1)}; otherwise, \code{(fitt-u)/fitt} where \code{fitt} is the estimated output evaluated at the frontier, given the inputs.

}
\value{
An object of class \code{semsfa} containing the following additional results:
\item{u}{the prediction of the individual efficiency score}
\item{efficiencies}{point estimate of the efficiency}

}
\references{
Jondrow, J., Lovell, C.A.K., Materov, I.S., Schmidt, P., 1982. On the estimation of technical inefficiency in
stochastic frontier production models. Journal of Econometrics 19, 233-238.

Kumbhakar, S.C., Lovell, C.A.K., 2000. Stochastic Frontier Analysis. Cambridge University Press, New York.
}
\author{
Giancarlo Ferrara and Francesco Vidoli
}


\seealso{
\code{\link{semsfa}}, \code{\link{summary.semsfa}}, \code{\link{plot.semsfa}}.
}
\examples{
set.seed(0)
n<-200

#generate data
x<- runif(n, 1, 2)
fy<- 2+30*x-5*x^2
v<- rnorm(n, 0, 1)
u<- abs(rnorm(n,0,2.5))
#production frontier
y <- fy + v - u
dati<-data.frame(y,x)

#first-step: gam, second-step: fan (default)
o<-semsfa(y~s(x),dati,sem.method="gam")

#calculate efficiencies
a<-efficiencies.semsfa(o)  
}