% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workhorses.R
\docType{methods}
\name{dprior}
\alias{dprior}
\alias{dprior,ANY-method}
\alias{dprior,missing-method}
\alias{dprior,pomp-method}
\title{dprior workhorse}
\usage{
\S4method{dprior}{pomp}(object, ..., params = coef(object), log = FALSE)
}
\arguments{
\item{object}{an object of class \sQuote{pomp}, or of a class that extends \sQuote{pomp}.
This will typically be the output of \code{pomp}, \code{simulate}, or one of the \pkg{pomp} inference algorithms.}

\item{...}{additional arguments are ignored.}

\item{params}{a \code{npar} x \code{nrep} matrix of parameters.
Each column is treated as an independent parameter set, in correspondence with the corresponding column of \code{x}.}

\item{log}{if TRUE, log probabilities are returned.}
}
\value{
The required density (or log density), as a numeric vector.
}
\description{
Evaluates the prior probability density.
}
\seealso{
Specification of the prior density evaluator: \link{prior_spec}

More on \pkg{pomp} workhorse functions: 
\code{\link{dinit}()},
\code{\link{dmeasure}()},
\code{\link{dprocess}()},
\code{\link{emeasure}()},
\code{\link{flow}()},
\code{\link{partrans}()},
\code{\link{pomp-package}},
\code{\link{rinit}()},
\code{\link{rmeasure}()},
\code{\link{rprior}()},
\code{\link{rprocess}()},
\code{\link{skeleton}()},
\code{\link{vmeasure}()},
\code{\link{workhorses}}

More on Bayesian methods:
\code{\link{abc}()},
\code{\link{bsmc2}()},
\code{\link{pmcmc}()},
\code{\link{prior_spec}},
\code{\link{rprior}()}
}
\concept{Bayesian methods}
\concept{extending the pomp package}
\concept{low-level interface}
\concept{pomp workhorses}
