##' prior specification
##'
##' Specify the prior distribution
##'
##' @name prior_spec
##' @rdname prior_spec
##' @family implementation_info
##'
##' @inheritSection pomp Note for Windows users
##' 
##' @details
##' A prior distribution on parameters is specified by means of the \code{rprior} and/or \code{dprior} arguments to \code{pomp}.
##' As with the other \link[=basic_components]{basic model components}, it is preferable to specify these using C snippets.
##' In writing a C snippet for the prior sampler (\code{rprior}), keep in mind that:
##' \enumerate{
##'   \item Within the context in which the snippet will be evaluated, only the parameters will be defined.
##'   \item The goal of such a snippet is the replacement of parameters with values drawn from the prior distribution.
##'   \item Hyperparameters can be included in the ordinary parameter list.
##'   Obviously, hyperparameters should not be replaced with random draws.
##' }
##' In writing a C snippet for the prior density function (\code{dprior}), observe that:
##' \enumerate{
##'   \item Within the context in which the snippet will be evaluated, only the parameters and \code{give_log} will be defined.
##'   \item The goal of such a snippet is computation of the prior probability density, or the log of same, at a given point in parameter space.
##'   This scalar value should be returned in the variable \code{lik}.
##'   When \code{give_log == 1}, \code{lik} should contain the log of the prior probability density.
##'   \item Hyperparameters can be included in the ordinary parameter list.
##' }
##' \link[=Csnippet]{General rules for writing C snippets can be found here}.
##' 
##' Alternatively, one can furnish \R functions for one or both of these arguments.
##' In this case, \code{rprior} must be a function of prototype \preformatted{
##'   f(params, \dots)}
##' that makes a draw from the prior distribution given \code{params} and returns a named vector of the same length and with the same set of names, as \code{params}.
##' The \code{dprior} function must be of prototype \preformatted{
##'   f(params, log = FALSE, \dots).}
##' Its role is to evaluate the prior probability density (or log density if \code{log == TRUE}) and return that single scalar value.
##'
##' @section Default behavior:
##' By default, the prior is assumed flat and improper.
##' In particular, \code{dprior} returns \code{1} (\code{0} if \code{log = TRUE}) for every parameter set.
##' Since it is impossible to simulate from a flat improper prior, \code{rprocess} returns missing values (\code{NA}s).
##'
NULL
