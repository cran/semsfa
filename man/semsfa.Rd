\name{semsfa}
\alias{semsfa}
\title{Semiparametric Estimation of Stochastic Frontier Models}
\description{
Semiparametric Estimation of Stochastic Frontier Models following the two step procedure proposed by Fan et al (1996) and further developed by Ferrara and Vidoli (2017). In the first step semiparametric or nonparametric regression techniques are used to relax parametric restrictions regards the functional form of the frontier and in the second step variance parameters are obtained by pseudolikelihood or method of moments estimators.
}
\usage{
semsfa(formula, data = list(), sem.method = "gam", var.method = "fan",
       ineffDecrease=TRUE, tol = 1e-05, n.boot=0,...)
}
\arguments{
  \item{formula}{
an object of class "formula": a symbolic description of the model to be fitted. The details of model specification are given under 'Details' 
}
  \item{data}{
a data frame containing the variables in the model
}
  \item{sem.method}{
a character string indicating the type of estimation method to be used in the first step for the semiparametric or nonparametric regression; possible values are "gam" (default), "kernel" or "loess"
}
  \item{var.method}{
the type of estimation method to be used in the second step for the variance components: "fan" (default) for Fan et al. (1996) approach and "mm" for method of moments
}
  \item{ineffDecrease}{
logical: TRUE (default) for estimating a production function, FALSE for estimating a cost function; this is done for usage compatibility with frontier package}

  \item{tol}{
numeric. Convergence tolerance for pseudolikelihood estimators of variance parameters of the composed error term
}
  \item{n.boot}{
numeric. Number of bootstrap replicates to calculate standard error for the variance components, by default bootstrap standard errors will not be calculated (n.boot=0)}

  \item{\dots}{
further arguments accepted by \code{mgcv::gam}, \code{np::npreg} or \code{loess}
}
}
\details{
Parametric stochastic production frontier models, introduced by Aigner et al. (1977) and Meeusen and van den Broeck (1977), specify output in terms of a response function and a composite error term. The composite error term consists of a two-sided error representing random effects and a one-sided term representing technical inefficiency.
The production stochastic frontier model can be written, in general terms, as:
\deqn{y_i = f (x_i)+v_i - u_i,\quad \quad i = 1, ..., n,}
where \eqn{Y_i\in R^+} is the single output of unit \eqn{i}, \eqn{X_i\in R^{+}_{p}} is the vector of inputs, \eqn{f(.)} defines a production frontier relationship between inputs X and the single output Y. In following common practice, we assume that \eqn{v} and \eqn{u} are each identically independently distributed (\eqn{iid}) with  \eqn{v\sim N(0,\sigma_v)} and  \eqn{u} distributed half-normally on the non-negative part of the real number line: \eqn{u\sim N^{+}(0,\sigma_u)}; furthermore, the probability density function of the composite disturbance can be rewritten in terms of \eqn{\lambda = \sigma_u/\sigma_v} and \eqn{\sigma^2 = \sigma_v^2+\sigma_u^2} for the estimation algorithm.
To overcome drawbacks due to the specification of a particular production function \eqn{f(\cdot)} we consider the estimation of a Semiparametric Stochastic Production Frontier Models through a two step procedure originally proposed by Fan et al (1996): in the first step a semiparametric or nonparametric regression technique is used to estimate the conditional expectation, while in the second step \eqn{\lambda} and \eqn{\sigma} parameters are estimated by pseudolikelihood (via optimize) or by method of moments estimators (var.method argument). 
In the case of a cost function frontier (\code{ineffDecrease=FALSE}) the composite error term is \eqn{\varepsilon = v + u}.
Vidoli and Ferrara (2014) and Ferrara and Vidoli(2017) suggest a Generalized Additive Model (GAM) framework in the first step even if any semiparametric or nonparametric tecnique may be used (Fan et al., 1996); for a recent review of SF analysis using R see Ferrara (2020).
 
The avalaible methods for the first step are:
\itemize{ 
\item sem.method="gam" invokes gam() from mgcv;
\item sem.method="gam.mono" invokes gamlss() from gamlss;
\item sem.method="kernel" invokes npreg() from np;
\item sem.method="loess" invokes loess() from stats.}
Since in the first step different estimation procedure may be invoked from different packages, the formula argument has to be compatible with the corresponding function.
The avalaible methods for the second step are:
\itemize{ 
\item var.method="fan" pseudolikelihood;
\item var.method="mm" Method of Moments.}
}
\value{
\code{semsfa()} returns an object of class \code{semsfa}. 
An \code{semsfa} object is a list containing the following components:
\item{formula}{the formula used}
\item{y}{the response variable used as specified in \code{formula}}
\item{data}{the data frame used}
\item{call}{the matched call}
\item{sem.method}{the type of semiparametric or nonparametric regression as given by sem.method ("gam", "gam.mono", "kernel", "loess")}
\item{var.method}{the type of error component estimator ("fan", "mm")}
\item{ineffDecrease}{logical, as given by ineffDecrease}
\item{reg}{an object of class "gam", "gamlss"(gam.mono), "np"(kernel) or "loess" depending on sem.method}
\item{reg.fitted}{fitted values on the "mean" frontier (semiparametric/non parametric regression)}
\item{regkewness}{asymmetry index calculated on residuals obtained in the first step}
\item{lambda}{\eqn{\lambda} estimate}
\item{sigma}{\eqn{\sigma} estimate}
\item{fitted}{fitted values on the frontier}
\item{tol}{convergence tolerance for pseudolikelihood estimators used in optimize}
\item{residual.df}{residual degree of freedom of the model}
\item{bic}{'Bayesian Information Criterion' according to the formula \code{-2*log-likelihood}\code{+ log(n)*npar} where \code{npar} represents the number of parameters in the fitted model and \code{n} the number of observations}
\item{n.boot}{number of bootstrap replicates used (default n.boot=0)}
\item{boot.mat}{a matrix containing \eqn{\lambda} and \eqn{\sigma} values from each bootstrap replicate (if n.boot>0)}
\item{b.se}{boostrapped standard errors for \eqn{\lambda} and \eqn{\sigma} (if n.boot>0)}
}
\note{
The function \code{summary} (i.e. \code{summary.semsfa}) can be used to obtain a summary of the results, \code{efficiencies.semsfa} to calculate efficiency scores and \code{plot} (i.e. \code{plot.semsfa}) to graph efficiency previsions and regression components (i.e. the first step).

You must take the natural logarithm of the response variable before fitting a stochastic frontier production or cost model.
}
\references{
Aigner., D., Lovell, C.A.K., Schmidt, P., 1977. Formulation and estimation of stochastic frontier production function models. Journal of Econometrics 6:21-37

Fan, Y., Li, Q., Weersink, A., 1996. Semiparametric estimation of stochastic production frontier models. Journal of Business & Economic Statistics 14:460-468

Ferrara G. (2020). Stochastic frontier models using R. In: Handbook of Statistics 42:299-326. Financial, Macro and Micro Econometrics Using R, Edited by Vinod and Rao. Elsevier

Ferrara G., Vidoli F. (2017). Semiparametric stochastic frontier models: A generalized additive model approach. European Journal of Operational Research 258:761-777.

Hastie, T., Tibshirani, R., 1990. Generalized additive models. Chapman & Hall

Kumbhakar, S.C., Lovell, C.A.K, 2000. Stochastic Frontier Analysis. Cambridge University Press, U.K

Meeusen, W., van den Broeck, J., 1977. Efficiency estimation from cobb-douglas production functions with composed error. International Economic Review 18:435-444

Vidoli, F., Ferrara, G., 2014. Analyzing Italian citrus sector by semi-nonparametric frontier efficiency models. Empirical Economics doi 10.1007/s00181-014-0879-6
}
\author{
Giancarlo Ferrara and Francesco Vidoli
}

\seealso{
\code{\link{summary.semsfa}}, \code{\link{efficiencies.semsfa}}, \code{\link{plot.semsfa}}.
}
\examples{  
set.seed(0)
n<-200

x<- runif(n, 1, 2)
v<- rnorm(n, 0, 1)
u<- abs(rnorm(n,0,2.5))

#cost frontier
fy<- 2+30*x+5*x^2
y <- fy + v + u

dati<-data.frame(y,x)
#first-step: gam, second-step: fan
o<-semsfa(y~s(x),dati,sem.method="gam",ineffDecrease=FALSE)
#first-step: gam, second-step: mm
\dontrun{o<-semsfa(y~s(x),dati,sem.method="gam",ineffDecrease=FALSE,var.method="mm")}
plot(x,y)
curve(2+30*x+5*x^2,add=TRUE)
points(sort(x),o$fitted[order(x)],col=3,type="l")

#production frontier
fy<- 2+30*x-5*x^2
y <- fy + v - u

dati<-data.frame(y,x)
#first-step: gam, second-step: fan
o<-semsfa(y~s(x),dati,sem.method="gam",ineffDecrease=TRUE)

plot(x,y)
curve(2+30*x-5*x^2,add=TRUE)
points(sort(x),o$fitted[order(x)],col=3,type="l")
}