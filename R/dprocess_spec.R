##' dprocess specification
##'
##' Specification of the latent state process density function, dprocess.
##'
##' @name dprocess_spec
##' @rdname dprocess_spec
##' @family implementation information
##' @seealso \code{\link{dprocess}}
##'
##' @details
##' Suppose you have a procedure that allows you to compute the probability density
##' of an arbitrary transition from state \eqn{x_1}{x1} at time \eqn{t_1}{t1}
##' to state \eqn{x_2}{x2} at time \eqn{t_2>t_1}{t2}
##' under the assumption that the state remains unchanged
##' between \eqn{t_1}{t1} and \eqn{t_2}{t2}.
##' Then you can furnish
##' \preformatted{
##'     dprocess = f
##' }
##' to \code{pomp}, where \code{f} is a C snippet or \R function that implements your procedure.
##' Specifically, \code{f} should compute the \emph{log} probability density.
##'
##' Using a C snippet is much preferred, due to its much greater computational efficiency.
##' See \code{\link{Csnippet}} for general rules on writing C snippets.
##' The goal of a \dfn{dprocess} C snippet is to fill the variable \code{loglik} with the log probability density.
##' In the context of such a C snippet, the parameters, and covariates will be defined, as will the times \code{t_1} and \code{t_2}.
##' The state variables at time \code{t_1} will have their usual name (see \code{statenames}) with a \dQuote{\code{_1}} appended.
##' Likewise, the state variables at time \code{t_2} will have a \dQuote{\code{_2}} appended.
##'
##' If \code{f} is given as an \R function, it should take as arguments any or all of the state variables, parameter, covariates, and time.
##' The state-variable and time arguments will have suffices \dQuote{\code{_1}} and \dQuote{\code{_2}} appended.
##' Thus for example, if \code{var} is a state variable, when \code{f} is called, \code{var_1} will value of state variable \code{var} at time \code{t_1}, \code{var_2} will have the value of \code{var} at time \code{t_2}.
##' \code{f} should return the \emph{log} likelihood of a transition from \code{x1} at time \code{t1} to \code{x2} at time \code{t2},
##' assuming that no intervening transitions have occurred.
##'
##'   To see examples, consult the demos and the tutorials on the \href{https://kingaa.github.io/pomp/}{package website}.
##'
##' @section \strong{Note}:
##' It is not typically necessary (or even feasible) to define \code{dprocess}.
##' In fact, no current \pkg{pomp} inference algorithm makes use of \code{dprocess}.
##' This functionality is provided only to support future algorithm development.
##'
##' @section Default behavior:
##' By default, \code{dprocess} returns missing values (\code{NA}).
##'
##' @inheritSection pomp Note for Windows users
##'
NULL
