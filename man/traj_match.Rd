% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traj_match.R
\docType{methods}
\name{traj_match}
\alias{traj_match}
\alias{traj_objfun}
\alias{traj_objfun,missing-method}
\alias{traj_objfun,ANY-method}
\alias{traj_objfun,data.frame-method}
\alias{traj_objfun,pomp-method}
\alias{traj_objfun,traj_match_objfun-method}
\title{Trajectory matching}
\usage{
\S4method{traj_objfun}{data.frame}(
  data,
  ...,
  est = character(0),
  fail.value = NA,
  ode_control = list(),
  params,
  rinit,
  skeleton,
  dmeasure,
  partrans,
  verbose = getOption("verbose", FALSE)
)

\S4method{traj_objfun}{pomp}(
  data,
  ...,
  est = character(0),
  fail.value = NA,
  ode_control = list(),
  verbose = getOption("verbose", FALSE)
)

\S4method{traj_objfun}{traj_match_objfun}(
  data,
  ...,
  est,
  fail.value,
  ode_control,
  verbose = getOption("verbose", FALSE)
)
}
\arguments{
\item{data}{either a data frame holding the time series data,
or an object of class \sQuote{pomp},
i.e., the output of another \pkg{pomp} calculation.
Internally, \code{data} will be coerced to an array with storage-mode \code{double}.}

\item{\dots}{additional arguments will modify the model structure}

\item{est}{character vector; the names of parameters to be estimated.}

\item{fail.value}{optional numeric scalar;
if non-\code{NA}, this value is substituted for non-finite values of the objective function.
It should be a large number (i.e., bigger than any legitimate values the objective function is likely to take).}

\item{ode_control}{optional list;
the elements of this list will be passed to \code{\link[deSolve]{ode}} if the skeleton is a vectorfield, and ignored if it is a map.}

\item{params}{optional; named numeric vector of parameters.
This will be coerced internally to storage mode \code{double}.}

\item{rinit}{simulator of the initial-state distribution.
This can be furnished either as a C snippet, an \R function, or the name of a pre-compiled native routine available in a dynamically loaded library.
Setting \code{rinit=NULL} sets the initial-state simulator to its default.
For more information, see \link[=rinit_spec]{rinit specification}.}

\item{skeleton}{optional; the deterministic skeleton of the unobserved state process.
Depending on whether the model operates in continuous or discrete time, this is either a vectorfield or a map.
Accordingly, this is supplied using either the \code{\link[=skeleton_spec]{vectorfield}} or \code{\link[=skeleton_spec]{map}} fnctions.
For more information, see \link[=skeleton_spec]{skeleton specification}.
Setting \code{skeleton=NULL} removes the deterministic skeleton.}

\item{dmeasure}{evaluator of the measurement model density, specified either as a C snippet, an \R function, or the name of a pre-compiled native routine available in a dynamically loaded library.
Setting \code{dmeasure=NULL} removes the measurement density evaluator.
For more information, see \link[=dmeasure_spec]{dmeasure specification}.}

\item{partrans}{optional parameter transformations, constructed using \code{\link{parameter_trans}}.

Many algorithms for parameter estimation search an unconstrained space of parameters.
When working with such an algorithm and a model for which the parameters are constrained, it can be useful to transform parameters.
One should supply the \code{partrans} argument via a call to \code{\link{parameter_trans}}.
For more information, see \link[=parameter_trans]{parameter_trans}.
Setting \code{partrans=NULL} removes the parameter transformations, i.e., sets them to the identity transformation.}

\item{verbose}{logical; if \code{TRUE}, diagnostic messages will be printed to the console.}
}
\value{
\code{traj_objfun} constructs a stateful objective function for spectrum matching.
Specifically, \code{traj_objfun} returns an object of class \sQuote{traj_match_objfun}, which is a function suitable for use in an \code{\link[stats]{optim}}-like optimizer.
In particular, this function takes a single numeric-vector argument that is assumed to contain the parameters named in \code{est}, in that order.
When called, it will return the negative log likelihood.
It is a stateful function:
Each time it is called, it will remember the values of the parameters and its estimate of the log likelihood.
}
\description{
Estimation of parameters for deterministic \acronym{POMP} models via trajectory matching.
}
\details{
In trajectory matching, one attempts to minimize the discrepancy between a \acronym{POMP} model's predictions and data under the assumption that the latent state process is deterministic and all discrepancies between model and data are due to measurement error.
The measurement model likelihood (\code{dmeasure}), or rather its negative, is the natural measure of the discrepancy.

Trajectory matching is a generalization of the traditional nonlinear least squares approach.
In particular, if, on some scale, measurement errors are normal with constant variance, then trajectory matching is equivalent to least squares on that particular scale.

\code{traj_objfun} constructs an objective function that evaluates the likelihood function.
It can be passed to any one of a variety of numerical optimization routines, which will adjust model parameters to minimize the discrepancies between the power spectrum of model simulations and that of the data.
}
\section{Note for Windows users}{


Some Windows users report problems when using C snippets in parallel computations.
These appear to arise when the temporary files created during the C snippet compilation process are not handled properly by the operating system.
To circumvent this problem, use the \code{\link[=pomp]{cdir}} and \code{\link[=pomp]{cfile}} options to cause the C snippets to be written to a file of your choice, thus avoiding the use of temporary files altogether.
}

\section{Important Note}{

Since \pkg{pomp} cannot guarantee that the \emph{final} call an optimizer makes to the function is a call \emph{at} the optimum, it cannot guarantee that the parameters stored in the function are the optimal ones.
Therefore, it is a good idea to evaluate the function on the parameters returned by the optimization routine, which will ensure that these parameters are stored.
}

\section{Warning! Objective functions based on C snippets}{

If you use C snippets (see \code{\link{Csnippet}}), a dynamically loadable library will be built.
As a rule, \pkg{pomp} functions load this library as needed and unload it when it is no longer needed.
The stateful objective functions are an exception to this rule.
For efficiency, calls to the objective function do not execute \code{\link{pompLoad}} or \code{\link{pompUnload}}:
rather, it is assumed that \code{\link{pompLoad}} has been called before any call to the objective function.
When a stateful objective function using one or more C snippets is created, \code{\link{pompLoad}} is called internally to build and load the library:
therefore, within a single \R session, if one creates a stateful objective function, one can freely call that objective function and (more to the point) pass it to an optimizer that calls it freely, without needing to call \code{\link{pompLoad}}.
On the other hand, if one retrieves a stored objective function from a file, or passes one to another \R session, one must call \code{\link{pompLoad}} before using it.
\strong{Failure to do this will typically result in a segmentation fault (i.e., it will crash the \R session).}
}

\examples{
\donttest{

  ricker() |>
    traj_objfun(
      est=c("r","sigma","N_0"),
      partrans=parameter_trans(log=c("r","sigma","N_0")),
      paramnames=c("r","sigma","N_0")
      ) -> f

  f(log(c(20,0.3,10)))

  if (require(subplex)) {
    subplex(fn=f,par=log(c(20,0.3,10)),control=list(reltol=1e-5)) -> out
  } else {
    optim(fn=f,par=log(c(20,0.3,10)),control=list(reltol=1e-5)) -> out
  }

  f(out$par)

  if (require(ggplot2)) {

    f |>
      trajectory(format="data.frame") |>
      ggplot(aes(x=time,y=N))+geom_line()+theme_bw()

  }

}
}
\seealso{
\code{\link[stats]{optim}}, \code{\link[subplex]{subplex}}, \code{\link[nloptr]{nloptr}}

More on methods for deterministic process models: 
\code{\link{flow}()},
\code{\link{skeleton}()},
\code{\link{skeleton_spec}},
\code{\link{trajectory}()}

More on maximization-based estimation methods:
\code{\link{mif2}()},
\code{\link{nlf}},
\code{\link{probe_match}},
\code{\link{spect_match}}
}
\concept{deterministic methods}
\concept{methods based on maximization}
\concept{trajectory matching}
