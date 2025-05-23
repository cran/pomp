% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wpfilter.R
\name{wpfilter}
\alias{wpfilter}
\alias{wpfilter,ANY-method}
\alias{wpfilter,missing-method}
\alias{wpfilter,data.frame-method}
\alias{wpfilter,pomp-method}
\alias{wpfilter,wpfilterd_pomp-method}
\title{Weighted particle filter}
\usage{
\S4method{wpfilter}{data.frame}(
  data,
  ...,
  Np,
  params,
  rinit,
  rprocess,
  dmeasure,
  trigger = 1,
  target = 0.5,
  verbose = getOption("verbose", FALSE)
)

\S4method{wpfilter}{pomp}(
  data,
  ...,
  Np,
  trigger = 1,
  target = 0.5,
  verbose = getOption("verbose", FALSE)
)

\S4method{wpfilter}{wpfilterd_pomp}(data, ..., Np, trigger, target, verbose = getOption("verbose", FALSE))
}
\arguments{
\item{data}{either a data frame holding the time series data,
or an object of class \sQuote{pomp},
i.e., the output of another \pkg{pomp} calculation.
Internally, \code{data} will be coerced to an array with storage-mode \code{double}.}

\item{...}{additional arguments are passed to \code{\link{pomp}}.
This allows one to set, unset, or modify \link[=basic_components]{basic model components} within a call to this function.}

\item{Np}{the number of particles to use.
This may be specified as a single positive integer, in which case the same number of particles will be used at each timestep.
Alternatively, if one wishes the number of particles to vary across timesteps, one may specify \code{Np} either as a vector of positive integers of length \preformatted{length(time(object,t0=TRUE))} or as a function taking a positive integer argument.
In the latter case, \code{Np(k)} must be a single positive integer, representing the number of particles to be used at the \code{k}-th timestep:
\code{Np(0)} is the number of particles to use going from \code{timezero(object)} to \code{time(object)[1]},
\code{Np(1)}, from \code{timezero(object)} to \code{time(object)[1]},
and so on,
while when \code{T=length(time(object))}, \code{Np(T)} is the number of particles to sample at the end of the time-series.}

\item{params}{optional; named numeric vector of parameters.
This will be coerced internally to storage mode \code{double}.}

\item{rinit}{simulator of the initial-state distribution.
This can be furnished either as a C snippet, an \R function, or the name of a pre-compiled native routine available in a dynamically loaded library.
Setting \code{rinit=NULL} sets the initial-state simulator to its default.
For more information, see \link[=rinit_spec]{rinit specification}.}

\item{rprocess}{simulator of the latent state process, specified using one of the \link[=rprocess_spec]{rprocess plugins}.
Setting \code{rprocess=NULL} removes the latent-state simulator.
For more information, see \link[=rprocess_spec]{rprocess specification for the documentation on these plugins}.}

\item{dmeasure}{evaluator of the measurement model density, specified either as a C snippet, an \R function, or the name of a pre-compiled native routine available in a dynamically loaded library.
Setting \code{dmeasure=NULL} removes the measurement density evaluator.
For more information, see \link[=dmeasure_spec]{dmeasure specification}.}

\item{trigger}{numeric; if the effective sample size becomes smaller than \code{trigger * Np}, resampling is triggered.}

\item{target}{numeric; target power.}

\item{verbose}{logical; if \code{TRUE}, diagnostic messages will be printed to the console.}
}
\value{
An object of class \sQuote{wpfilterd_pomp}, which extends class \sQuote{pomp}.
Information can be extracted from this object using the methods documented below.
}
\description{
A sequential importance sampling (particle filter) algorithm.
Unlike in \code{pfilter}, resampling is performed only when triggered by
deficiency in the effective sample size.
}
\details{
\bold{This function is experimental and should be considered in alpha stage.
Both interface and underlying algorithms may change without warning at any time.
Please explore the function and give feedback via the \pkg{pomp} Issues page.}
}
\section{Methods}{

\describe{
\item{\code{\link{logLik}}}{ the estimated log likelihood}
\item{\code{\link{cond_logLik}}}{ the estimated conditional log likelihood}
\item{\code{\link{eff_sample_size}}}{the (time-dependent) estimated effective sample size}
\item{\code{\link{as.data.frame}}}{ coerce to a data frame}
\item{\code{\link{plot}}}{diagnostic plots}
}
}

\section{Note for Windows users}{


Some Windows users report problems when using C snippets in parallel computations.
These appear to arise when the temporary files created during the C snippet compilation process are not handled properly by the operating system.
To circumvent this problem, use the \code{\link[=pomp]{cdir}} and \code{\link[=pomp]{cfile}} options to cause the C snippets to be written to a file of your choice, thus avoiding the use of temporary files altogether.
}

\references{
\Arulampalam2002
}
\seealso{
More on \pkg{pomp} elementary algorithms: 
\code{\link{elementary_algorithms}},
\code{\link{kalman}},
\code{\link{pfilter}()},
\code{\link{pomp-package}},
\code{\link{probe}()},
\code{\link{simulate}()},
\code{\link{spect}()},
\code{\link{trajectory}()}

More on sequential Monte Carlo methods: 
\code{\link{bsmc2}()},
\code{\link{cond_logLik}()},
\code{\link{eff_sample_size}()},
\code{\link{filter_mean}()},
\code{\link{filter_traj}()},
\code{\link{kalman}},
\code{\link{mif2}()},
\code{\link{pfilter}()},
\code{\link{pmcmc}()},
\code{\link{pred_mean}()},
\code{\link{pred_var}()},
\code{\link{saved_states}()}

More on full-information (i.e., likelihood-based) methods:
\code{\link{bsmc2}()},
\code{\link{mif2}()},
\code{\link{pfilter}()},
\code{\link{pmcmc}()}
}
\author{
Aaron A. King
}
\concept{elementary algorithms}
\concept{full-information methods}
\concept{particle filter methods}
