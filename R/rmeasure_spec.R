##' The measurement-model simulator
##'
##' Specification of rmeasure
##'
##' @inheritSection pomp Note for Windows users
##'
##' @name rmeasure_spec
##' @rdname rmeasure_spec
##' @family implementation_info
##'
##' @details
##' The measurement model is the link between the data and the unobserved state process.
##' It can be specified either by using one or both of the \code{rmeasure} and \code{dmeasure} arguments.
##'
##' Suppose you have a procedure to simulate observations given the value of the latent state variables.
##' Then you can furnish \preformatted{
##'   rmeasure = f}
##' to \pkg{pomp} algorithms,
##' where \code{f} is a C snippet or \R function that implements your procedure.
##'
##' Using a C snippet is much preferred, due to its much greater computational efficiency.
##' See \code{\link{Csnippet}} for general rules on writing C snippets.
##'
##' In writing an \code{rmeasure} C snippet, bear in mind that:
##'   \enumerate{
##'     \item The goal of such a snippet is to fill the observables with random values drawn from the measurement model distribution.
##'     Accordingly, each observable should be assigned a new value.
##'     \item In addition to the states, parameters, covariates (if any), and observables, the variable \code{t}, containing the time of the observation, will be defined in the context in which the snippet is executed.
##'   }
##'
##' The demos and the tutorials on the \href{https://kingaa.github.io/pomp/}{package website} give examples as well.
##'
##' It is also possible, though far less efficient, to specify \code{rmeasure} using an \R function.
##' In this case, specify the measurement model simulator by furnishing \preformatted{
##'   rmeasure = f}
##' to \code{pomp}, where \code{f} is an \R function.
##' The arguments of \code{f} should be chosen from among the state variables, parameters, covariates, and time.
##' It must also have the argument \code{...}.
##' \code{f} must return a named numeric vector of length equal to the number of observable variables.
##'
##' @section Default behavior:
##' The default \code{rmeasure} is undefined.
##' It will yield missing values (\code{NA}).
NULL
