% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{transformations}
\alias{transformations}
\alias{logit}
\alias{expit}
\alias{log_barycentric}
\alias{inv_log_barycentric}
\title{Transformations}
\usage{
logit(p)

expit(x)

log_barycentric(X)

inv_log_barycentric(Y)
}
\arguments{
\item{p}{numeric; a quantity in [0,1].}

\item{x}{numeric; the log odds ratio.}

\item{X}{numeric; a vector containing the quantities to be transformed according to the log-barycentric transformation.}

\item{Y}{numeric; a vector containing the log fractions.}
}
\description{
Some useful parameter transformations.
}
\details{
Parameter transformations can be used in many cases to recast constrained optimization problems as unconstrained problems.
Although there are no limits to the transformations one can implement using the \code{\link{parameter_trans}} facilty, \pkg{pomp} provides a few ready-built functions to implement some very commonly useful ones.

The logit transformation takes a probability \eqn{p} to its log odds, \eqn{\log\frac{p}{1-p}}{log(p/(1-p))}.
It maps the unit interval \eqn{[0,1]} into the extended real line \eqn{[-\infty,\infty]}.

The inverse of the logit transformation is the expit transformation.

The log-barycentric transformation takes a vector \eqn{X\in{R^{n}_+}}{X in the non-negative cone of R^n}, to a vector \eqn{Y\in{R^n}}{Y in R^n}, where \deqn{Y_i = \log\frac{X_i}{\sum_j X_j}.}{Yi = log(Xi/sum(X)).}
The transformation is not one-to-one.
However, for each \eqn{c>0}, it maps the simplex \eqn{\{X\in{R^n_+}:\sum_i X_i = c\}}{sum(X)=c} bijectively onto \eqn{n}-dimensional Euclidean space \eqn{R^n}.

The inverse of the log-barycentric transformation is implemented as \code{inv_log_barycentric}.
Note that it is not a true inverse, in the sense that it takes \eqn{R^n} to the \emph{unit} simplex, \eqn{\{X\in{R^n_+}:\sum_i X_i = 1\}}{sum(X)=1}.
Thus, \preformatted{
    log_barycentric(inv_log_barycentric(Y)) == Y,
} but \preformatted{
    inv_log_barycentric(log_barycentric(X)) == X
} only if \code{sum(X) == 1}.
}
\seealso{
More on implementing POMP models: 
\code{\link{Csnippet}},
\code{\link{accumvars}},
\code{\link{basic_components}},
\code{\link{betabinomial}},
\code{\link{covariates}},
\code{\link{dinit_spec}},
\code{\link{dmeasure_spec}},
\code{\link{dprocess_spec}},
\code{\link{emeasure_spec}},
\code{\link{eulermultinom}},
\code{\link{parameter_trans}()},
\code{\link{pomp-package}},
\code{\link{pomp_constructor}},
\code{\link{prior_spec}},
\code{\link{rinit_spec}},
\code{\link{rmeasure_spec}},
\code{\link{rprocess_spec}},
\code{\link{skeleton_spec}},
\code{\link{userdata}},
\code{\link{vmeasure_spec}}
}
\concept{implementation information}
\concept{parameter transformations}
