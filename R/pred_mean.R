##' Prediction mean
##'
##' The mean of the prediction distribution
##'
##' The prediction distribution is that of
##' \deqn{X(t_k) \vert Y(t_1)=y^*_1,\dots,Y(t_{k-1})=y^*_{k-1},}{Xk | Y1=y1*,\dots,Y(k-1)=y(k-1)*,}
##' where \eqn{X(t_k)}{Xk}, \eqn{Y(t_k)}{Yk} are the latent state and observable processes, respectively, and \eqn{y^*_k}{yk*} is the data, at time \eqn{t_k}{tk}.
##'
##' The prediction mean is therefore the expectation of this distribution
##' \deqn{E[X(t_k) \vert Y(t_1)=y^*_1,\dots,Y(t_{k-1})=y^*_{k-1}].}{E[Xk | Y1=y1*,\dots,Y(k-1)=y(k-1)*].}
##'
##' @name pred_mean
##' @aliases pred_mean,ANY-method pred_mean,missing-method
##' @include pfilter.R kalman.R melt.R
##' @rdname pred_mean
##' @family particle filter methods
##' @family extraction methods
##' @inheritParams filter_mean
##'
NULL

setGeneric(
  "pred_mean",
  function (object, ...)
    standardGeneric("pred_mean")
)

setMethod(
  "pred_mean",
  signature=signature(object="missing"),
  definition=function (...) {
    reqd_arg("pred_mean","object")
  }
)

setMethod(
  "pred_mean",
  signature=signature(object="ANY"),
  definition=function (object, ...) {
    undef_method("pred_mean",object)
  }
)

##' @rdname pred_mean
##' @export
setMethod(
  "pred_mean",
  signature=signature(object="kalmand_pomp"),
  definition=function (object, vars, ...,
    format = c("array", "data.frame")) {
    if (missing(vars)) {
      x <- object@pred.mean
    } else {
      x <- object@pred.mean[vars,,drop=FALSE]
    }
    format <- match.arg(format)
    if (format == "data.frame") {
      x <- melt(object@pred.mean[vars,,drop=FALSE])
      x$time <- time(object)[as.integer(x$time)]
    }
    x
  }
)

##' @rdname pred_mean
##' @export
setMethod(
  "pred_mean",
  signature=signature(object="pfilterd_pomp"),
  definition=function (object, vars, ...,
    format = c("array", "data.frame")) {
    if (missing(vars)) {
      x <- object@pred.mean
    } else {
      x <- object@pred.mean[vars,,drop=FALSE]
    }
    format <- match.arg(format)
    if (format == "data.frame") {
      x <- melt(object@pred.mean[vars,,drop=FALSE])
      x$time <- time(object)[as.integer(x$time)]
    }
    x
  }
)
